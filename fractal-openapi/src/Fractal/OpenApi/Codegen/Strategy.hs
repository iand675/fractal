-- | Pluggable code generation strategies
module Fractal.OpenApi.Codegen.Strategy
  ( -- * Strategy Types
    CodegenStrategy(..)
  , GeneratedType(..)
  , GeneratedField(..)
  
    -- * Built-in Strategies
  , defaultStrategy
  , newtypeStrategy
  , sumTypeStrategy
  , enumStrategy
  
    -- * Strategy Selection
  , selectStrategy
  ) where

import Fractal.OpenApi.Codegen.Core
import Fractal.JsonSchema.Types hiding (codegenStrictFields)
import Fractal.JsonSchema.Parser.Internal (parseSchemaValue)
import qualified Fractal.JsonSchema.Parser as Parser
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as AesonTypes
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH (Name, mkName)
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable (toList)

-- | Strategy for generating code from a schema
data CodegenStrategy
  = RecordStrategy      -- ^ Generate a record type
  | NewtypeStrategy     -- ^ Generate a newtype (single field optimization)
  | SumTypeStrategy     -- ^ Generate a sum type (oneOf/anyOf)
  | EnumStrategy        -- ^ Generate an ADT from string enum
  | PrimitiveStrategy   -- ^ Don't generate, use primitive type
  deriving (Eq, Show)

-- | A generated Haskell type
data GeneratedType = GeneratedType
  { genTypeName :: Text
    -- ^ Name of the generated type
  
  , genTypeConstructors :: NonEmpty (Text, [GeneratedField])
    -- ^ Constructors with their fields
  
  , genTypeDoc :: Maybe Text
    -- ^ Haddock documentation
  
  , genTypeStrict :: Bool
    -- ^ Whether fields should be strict
  
  , genTypeDeriving :: [Text]
    -- ^ Deriving clauses
  }
  deriving (Eq, Show)

-- | A field in a generated record
data GeneratedField = GeneratedField
  { fieldName :: Text
    -- ^ Field name
  
  , fieldType :: Text
    -- ^ Field type (as Text for now, could be TH Type later)
  
  , fieldOptional :: Bool
    -- ^ Whether wrapped in Maybe
  
  , fieldDoc :: Maybe Text
    -- ^ Haddock documentation for this field
  }
  deriving (Eq, Show)

-- | Parse properties from raw keywords
parsePropertiesFromRaw :: Schema -> Map Text Schema
parsePropertiesFromRaw schema = 
  case Map.lookup "properties" (schemaRawKeywords schema) of
    Just (Object propsObj) ->
      let version = fromMaybe Draft202012 (schemaVersion schema)
          entries = KeyMap.toList propsObj
          parseEntry (k, v) = case parseSchemaValue version v of
            Right s -> Just (Key.toText k, s)
            Left _ -> Nothing
      in Map.fromList $ mapMaybe parseEntry entries
    _ -> Map.empty

-- | Parse required from raw keywords
parseRequiredFromRaw :: Schema -> Set Text
parseRequiredFromRaw schema =
  case Map.lookup "required" (schemaRawKeywords schema) of
    Just (Array arr) -> Set.fromList [t | String t <- toList arr]
    _ -> Set.empty

-- | Select appropriate strategy for a schema
selectStrategy :: CodegenConfig -> Schema -> CodegenStrategy
selectStrategy config schema =
  case schemaCore schema of
    BooleanSchema _ -> PrimitiveStrategy
    ObjectSchema obj ->
      -- Check for x-newtype annotation
      case Map.lookup "x-newtype" (schemaExtensions schema) of
        Just _ -> NewtypeStrategy
        Nothing ->
          -- Check for oneOf/anyOf (sum type)
          case (schemaOneOf obj, schemaAnyOf obj) of
            (Just _, _) -> SumTypeStrategy
            (_, Just _) -> SumTypeStrategy
            _ ->
              -- Check for enum
              case schemaEnum obj of
                Just _ -> EnumStrategy
                Nothing ->
                  -- Parse properties and required on-demand from raw keywords
                  let props = parsePropertiesFromRaw schema
                      required = parseRequiredFromRaw schema
                  in if codegenNewtypeOptimization config && 
                        Map.size props == 1 && 
                        Set.size required == 1
                     then NewtypeStrategy
                     else RecordStrategy

-- | Default strategy: generate record types
defaultStrategy :: CodegenConfig -> CodegenContext -> Either Text GeneratedType
defaultStrategy config ctx =
  let schema = codegenSchema ctx
  in case schemaCore schema of
    BooleanSchema _ -> Left "Cannot generate type from boolean schema"
    ObjectSchema obj ->
      let props = parsePropertiesFromRaw schema
          required = parseRequiredFromRaw schema
          
          -- Generate fields
          fields = mapMaybe (generateField config required) (Map.toList props)
          
          -- Type name
          typeName = generateTypeName config ctx
          
          -- Documentation
          doc = annotationTitle (schemaAnnotations obj) <> 
                fmap (\d -> "\n\n" <> d) (annotationDescription $ schemaAnnotations obj)
      
      in case NE.nonEmpty fields of
        Nothing -> Left "Cannot generate record with no fields"
        Just _ ->
          Right $ GeneratedType
            { genTypeName = typeName
            , genTypeConstructors = NE.singleton (typeName, fields)
            , genTypeDoc = doc
            , genTypeStrict = codegenStrictFields config
            , genTypeDeriving = ["Eq", "Show", "Generic"]
            }
  where
    generateField :: CodegenConfig -> Set Text -> (Text, Schema) -> Maybe GeneratedField
    generateField cfg requiredFields (propName, propSchema) =
      let fieldName' = generateFieldName (conventionFieldNaming $ codegenNaming cfg) propName
          isRequired = Set.member propName requiredFields
          fieldType' = schemaToHaskellType propSchema
          fieldDoc' = case schemaCore propSchema of
            ObjectSchema obj -> annotationDescription (schemaAnnotations obj)
            _ -> Nothing
      in Just $ GeneratedField
        { fieldName = fieldName'
        , fieldType = if isRequired then fieldType' else "Maybe (" <> fieldType' <> ")"
        , fieldOptional = not isRequired
        , fieldDoc = fieldDoc'
        }
    
    schemaToHaskellType :: Schema -> Text
    schemaToHaskellType sch = case schemaCore sch of
      BooleanSchema _ -> "Value"  -- Fallback to Aeson Value
      ObjectSchema obj ->
        case schemaType obj of
          Just (One typ) -> case typ of
            StringType -> "Text"
            NumberType -> "Scientific"
            IntegerType -> "Integer"
            BooleanType -> "Bool"
            ArrayType -> "[Value]"
            ObjectType -> "Object"
            NullType -> "()"
          Just (Many _) -> "Value"  -- Union type
          Nothing -> "Value"  -- No type specified

-- | Newtype strategy: generate a newtype wrapper
newtypeStrategy :: CodegenConfig -> CodegenContext -> Either Text GeneratedType
newtypeStrategy config ctx =
  let schema = codegenSchema ctx
  in case schemaCore schema of
    ObjectSchema obj ->
      let props = parsePropertiesFromRaw schema
          typeName = generateTypeName config ctx
          doc = annotationTitle (schemaAnnotations obj)
      in if Map.size props /= 1
         then Left "Newtype strategy requires exactly one field"
         else
           let (propName, propSchema) = head $ Map.toList props
               fieldType' = schemaToHaskellType propSchema
           in Right $ GeneratedType
             { genTypeName = typeName
             , genTypeConstructors = NE.singleton (typeName, [GeneratedField propName fieldType' False Nothing])
             , genTypeDoc = doc
             , genTypeStrict = True
             , genTypeDeriving = ["Eq", "Show", "Generic"]
             }
    _ -> Left "Newtype strategy requires object schema"
  where
    schemaToHaskellType :: Schema -> Text
    schemaToHaskellType sch = case schemaCore sch of
      ObjectSchema obj -> case schemaType obj of
        Just (One StringType) -> "Text"
        Just (One IntegerType) -> "Integer"
        Just (One NumberType) -> "Scientific"
        Just (One BooleanType) -> "Bool"
        _ -> "Value"
      _ -> "Value"

-- | Sum type strategy: generate ADT from oneOf/anyOf
sumTypeStrategy :: CodegenConfig -> CodegenContext -> Either Text GeneratedType
sumTypeStrategy config ctx =
  let schema = codegenSchema ctx
  in case schemaCore schema of
    ObjectSchema obj ->
      case schemaOneOf obj of
        Just alternatives ->
          let typeName = generateTypeName config ctx
              constructors = NE.zip (generateConstructorNames typeName alternatives) alternatives
              doc = annotationTitle (schemaAnnotations obj)
          in Right $ GeneratedType
            { genTypeName = typeName
            , genTypeConstructors = NE.map (\(name, _) -> (name, [])) constructors
            , genTypeDoc = doc
            , genTypeStrict = False
            , genTypeDeriving = ["Eq", "Show", "Generic"]
            }
        Nothing -> Left "Sum type strategy requires oneOf"
    _ -> Left "Sum type strategy requires object schema"
  where
    generateConstructorNames :: Text -> NonEmpty Schema -> NonEmpty Text
    generateConstructorNames base alts =
      NE.fromList [base <> T.pack (show i) | i <- [1..NE.length alts]]

-- | Enum strategy: generate ADT from string enum
enumStrategy :: CodegenConfig -> CodegenContext -> Either Text GeneratedType
enumStrategy config ctx =
  let schema = codegenSchema ctx
  in case schemaCore schema of
    ObjectSchema obj ->
      case schemaEnum obj of
        Just values ->
          let typeName = generateTypeName config ctx
              constructors = NE.map enumValueToConstructor values
              doc = annotationTitle (schemaAnnotations obj)
          in Right $ GeneratedType
            { genTypeName = typeName
            , genTypeConstructors = NE.map (\name -> (name, [])) constructors
            , genTypeDoc = doc
            , genTypeStrict = False
            , genTypeDeriving = ["Eq", "Show", "Ord", "Enum", "Bounded", "Generic"]
            }
        Nothing -> Left "Enum strategy requires enum values"
    _ -> Left "Enum strategy requires object schema"
  where
    enumValueToConstructor :: Value -> Text
    enumValueToConstructor (String txt) = sanitizeHaskellIdentifier True txt
    enumValueToConstructor val = "Enum" <> T.pack (show val)
