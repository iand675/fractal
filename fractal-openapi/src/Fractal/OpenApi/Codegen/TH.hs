{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Template Haskell code generation API
module Fractal.OpenApi.Codegen.TH
  ( -- * Main Generation Functions
    deriveJSONSchema
  , deriveJSONSchemaWith
  , deriveJSONSchemaFromFile
  
    -- * Low-level Generation
  , generateTypeDeclaration
  , generateHasSchemaInstance
  , generateDataDeclaration
  ) where

import Fractal.OpenApi.Codegen.Core
import Fractal.OpenApi.Codegen.Strategy hiding (NewtypeStrategy)
import qualified Fractal.OpenApi.Codegen.Strategy as Strategy
import Fractal.OpenApi.Codegen.Aeson (generateAesonInstances)
import Fractal.JsonSchema.Types
import Fractal.JsonSchema (parseSchema)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (Value, Object)
import qualified Data.Aeson as Aeson
import Data.Scientific (Scientific)
import Data.Maybe (catMaybes)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Control.Monad (forM)

-- | Derive a Haskell type from a JSON Schema value
-- 
-- Usage:
-- 
-- > deriveJSONSchema [schema| { "type": "object", "properties": { "name": { "type": "string" } } } |]
deriveJSONSchema :: Value -> Q [Dec]
deriveJSONSchema schemaValue = deriveJSONSchemaWith defaultCodegenConfig schemaValue

-- | Derive a Haskell type with custom configuration
deriveJSONSchemaWith :: CodegenConfig -> Value -> Q [Dec]
deriveJSONSchemaWith config schemaValue = do
  -- Parse the schema
  case parseSchema schemaValue of
    Left parseErr -> fail $ "Failed to parse schema: " <> show parseErr
    Right schema -> do
      -- Create codegen context
      let ctx = CodegenContext
            { codegenSchema = schema
            , codegenPath = emptyPointer
            , codegenRegistry = emptyTypeRegistry
            , codegenParentName = Nothing
            }
      
      -- Select strategy and generate
      let strategy = selectStrategy config schema
      case strategy of
        PrimitiveStrategy -> fail "Cannot generate type from primitive schema"
        RecordStrategy -> generateFromStrategy defaultStrategy config ctx
        Strategy.NewtypeStrategy -> generateFromStrategy Strategy.newtypeStrategy config ctx
        SumTypeStrategy -> generateFromStrategy sumTypeStrategy config ctx
        EnumStrategy -> generateFromStrategy enumStrategy config ctx

-- | Derive from a schema file
deriveJSONSchemaFromFile :: FilePath -> Q [Dec]
deriveJSONSchemaFromFile path = do
  -- Read and parse the file at compile time
  schemaValue <- runIO $ do
    result <- Aeson.eitherDecodeFileStrict path
    case result of
      Left err -> fail $ "Failed to parse schema file " <> path <> ": " <> err
      Right val -> pure val
  
  deriveJSONSchema schemaValue

-- | Generate from a strategy function
generateFromStrategy 
  :: (CodegenConfig -> CodegenContext -> Either Text GeneratedType)
  -> CodegenConfig
  -> CodegenContext
  -> Q [Dec]
generateFromStrategy strategyFn config ctx = do
  case strategyFn config ctx of
    Left err -> fail $ "Code generation failed: " <> T.unpack err
    Right genType -> do
      -- Generate the data declaration
      dataDecl <- generateDataDeclaration genType
      
      -- Generate Aeson instances (FromJSON/ToJSON)
      aesonInsts <- generateAesonInstances genType
      
      -- Generate HasSchema instance if requested
      hasSchemaInst <- if codegenGenerateHasSchema config
                       then generateHasSchemaInstance genType (codegenSchema ctx)
                       else pure []
      
      pure $ dataDecl : (aesonInsts ++ hasSchemaInst)

-- | Generate a data declaration from GeneratedType
generateDataDeclaration :: GeneratedType -> Q Dec
generateDataDeclaration genType = do
  let typeName = mkName $ T.unpack $ genTypeName genType
      deriving' = map (mkName . T.unpack) (genTypeDeriving genType)
  
  -- Generate constructors
  cons <- forM (toList $ genTypeConstructors genType) $ \(conName, fields) -> do
    let conName' = mkName $ T.unpack conName
    
    if null fields
      then -- Simple constructor with no fields
           pure $ NormalC conName' []
      else if length fields == 1 && genTypeStrict genType
           then -- Newtype-style (single field, strict)
                let field = head fields
                    fieldType' = textToType (fieldType field)
                in pure $ NormalC conName' [(Bang NoSourceUnpackedness SourceStrict, fieldType')]
           else -- Record constructor
                let recordFields = map generateRecordField fields
                in pure $ RecC conName' recordFields
  
  -- Create the data declaration
  pure $ DataD [] typeName [] Nothing cons [DerivClause Nothing (map ConT deriving')]
  where
    toList = NE.toList
    
    generateRecordField :: GeneratedField -> VarBangType
    generateRecordField field =
      let fieldName' = mkName $ T.unpack $ fieldName field
          fieldType' = textToType (fieldType field)
          strictness = if genTypeStrict genType
                       then Bang NoSourceUnpackedness SourceStrict
                       else Bang NoSourceUnpackedness NoSourceStrictness
      in (fieldName', strictness, fieldType')
    
    -- Convert Text type representation to TH Type
    textToType :: Text -> Type
    textToType txt = case T.unpack txt of
      "Text" -> ConT ''Text
      "String" -> ConT ''String
      "Int" -> ConT ''Int
      "Integer" -> ConT ''Integer
      "Bool" -> ConT ''Bool
      "Double" -> ConT ''Double
      "Scientific" -> ConT ''Scientific
      "Value" -> ConT ''Value
      "Object" -> ConT ''Object
      "()" -> TupleT 0
      -- Handle Maybe
      s | "Maybe (" `T.isPrefixOf` txt ->
          let inner = T.drop 7 $ T.dropEnd 1 txt
          in AppT (ConT ''Maybe) (textToType inner)
      -- Handle lists
      s | "[" `T.isPrefixOf` txt && "]" `T.isSuffixOf` txt ->
          let inner = T.drop 1 $ T.dropEnd 1 txt
          in AppT ListT (textToType inner)
      -- Fallback: assume it's a custom type
      s -> ConT (mkName s)

-- | Generate HasSchema instance
generateHasSchemaInstance :: GeneratedType -> Schema -> Q [Dec]
generateHasSchemaInstance genType schema = do
  let typeName = mkName $ T.unpack $ genTypeName genType
      
  -- schemaFor implementation
  schemaForDecl <- [d|
    instance HasSchema $(conT typeName) where
      schemaFor _ = $(lift schema)
    |]
  
  pure schemaForDecl

-- | Generate a complete type declaration with all instances
generateTypeDeclaration :: CodegenConfig -> Schema -> Q [Dec]
generateTypeDeclaration config schema = do
  let ctx = CodegenContext
        { codegenSchema = schema
        , codegenPath = emptyPointer
        , codegenRegistry = emptyTypeRegistry
        , codegenParentName = Nothing
        }
  
  deriveJSONSchemaWith config (Aeson.toJSON schema)
