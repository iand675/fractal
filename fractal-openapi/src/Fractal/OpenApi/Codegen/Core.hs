{-# LANGUAGE TemplateHaskell #-}

-- | Code generation core infrastructure
module Fractal.OpenApi.Codegen.Core
  ( -- * HasSchema Typeclass
    HasSchema(..)
  
    -- * Codegen Types
  , CodegenContext(..)
  , CodegenConfig(..)
  , defaultCodegenConfig
  , TypeRegistry(..)
  , emptyTypeRegistry
  , NamingConvention(..)
  , FieldNaming(..)
  , TypeNaming(..)
  
    -- * Name Generation
  , generateTypeName
  , generateFieldName
  , sanitizeHaskellIdentifier
  , isValidHaskellIdentifier
  
    -- * Utility Functions
  , validateDynamic
  , describeType
  ) where

import Fractal.JsonSchema.Types
import Data.Aeson (ToJSON, toJSON)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toUpper, toLower, isAlphaNum, isDigit, isLetter, isNumber)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Haskell.TH (Q, Name)
import GHC.Generics (Generic)

-- | Typeclass linking generated types back to their originating schema
class HasSchema a where
  -- | Retrieve the schema that this type was generated from
  schemaFor :: Proxy a -> Schema
  
  -- | Optionally provide the JSON Pointer to the schema location
  schemaPath :: Proxy a -> Maybe JsonPointer
  schemaPath _ = Nothing

-- | Validate a value against its schema
validateDynamic :: (HasSchema a, ToJSON a) => a -> ValidationResult
validateDynamic = error "Not yet implemented"  -- TODO: Implement in US4

-- | Get schema title/description
describeType :: HasSchema a => Proxy a -> Maybe Text
describeType = error "Not yet implemented"  -- TODO: Implement in US4

-- | Type registry to avoid duplicate generation
data TypeRegistry = TypeRegistry
  { registryTypes :: Map SchemaFingerprint Name
  , registryNames :: Set Text
  }
  deriving (Eq, Show, Generic)

instance Semigroup TypeRegistry where
  r1 <> r2 = TypeRegistry
    { registryTypes = registryTypes r1 <> registryTypes r2
    , registryNames = registryNames r1 <> registryNames r2
    }

instance Monoid TypeRegistry where
  mempty = emptyTypeRegistry

-- | Empty type registry
emptyTypeRegistry :: TypeRegistry
emptyTypeRegistry = TypeRegistry Map.empty Set.empty

-- | Naming conventions for generated code
data NamingConvention = NamingConvention
  { conventionFieldNaming :: FieldNaming
  , conventionTypeNaming :: TypeNaming
  , conventionStripPrefix :: Maybe Text
  , conventionAddPrefix :: Maybe Text
  }
  deriving (Eq, Show, Generic)

-- | Field naming strategy
data FieldNaming
  = FieldUnchanged          -- ^ Keep field names as-is
  | FieldCamelCase          -- ^ Convert to camelCase
  | FieldPascalCase         -- ^ Convert to PascalCase
  | FieldSnakeCase          -- ^ Convert to snake_case
  | FieldStripPrefix Text   -- ^ Strip a prefix
  deriving (Eq, Show, Generic)

-- | Type naming strategy
data TypeNaming
  = TypeFromTitle           -- ^ Use schema title
  | TypeFromProperty Text   -- ^ Use property name
  | TypeGenerated          -- ^ Generate from context
  deriving (Eq, Show, Generic)

-- | Context for code generation
data CodegenContext = CodegenContext
  { codegenSchema :: Schema
    -- ^ The schema being generated from
  
  , codegenPath :: JsonPointer
    -- ^ Path to this schema in the document
  
  , codegenRegistry :: TypeRegistry
    -- ^ Registry of already-generated types
  
  , codegenParentName :: Maybe Text
    -- ^ Parent type name for nested generation
  }
  deriving (Generic)

-- | Configuration for code generation
data CodegenConfig = CodegenConfig
  { codegenGenerateHasSchema :: Bool
    -- ^ Generate HasSchema instances
  
  , codegenGenerateValidation :: Bool
    -- ^ Embed validation in FromJSON instances
  
  , codegenNaming :: NamingConvention
    -- ^ Naming conventions
  
  , codegenStrictFields :: Bool
    -- ^ Add strictness annotations to fields
  
  , codegenGenerateHaddock :: Bool
    -- ^ Generate Haddock comments from schema metadata
  
  , codegenNewtypeOptimization :: Bool
    -- ^ Use newtype for single-field records
  }
  deriving (Eq, Show, Generic)

-- | Default code generation configuration
defaultCodegenConfig :: CodegenConfig
defaultCodegenConfig = CodegenConfig
  { codegenGenerateHasSchema = True
  , codegenGenerateValidation = False  -- Don't embed validation by default
  , codegenNaming = NamingConvention
      { conventionFieldNaming = FieldCamelCase
      , conventionTypeNaming = TypeFromTitle
      , conventionStripPrefix = Nothing
      , conventionAddPrefix = Nothing
      }
  , codegenStrictFields = True
  , codegenGenerateHaddock = True
  , codegenNewtypeOptimization = True
  }

-- | Generate a type name from a schema
generateTypeName :: CodegenConfig -> CodegenContext -> Text
generateTypeName config ctx =
  let naming = codegenNaming config
      baseName = case conventionTypeNaming naming of
        TypeFromTitle -> 
          case schemaCore (codegenSchema ctx) of
            ObjectSchema obj -> 
              case annotationTitle (schemaAnnotations obj) of
                Just title -> title
                Nothing -> generateFromPath ctx
            _ -> generateFromPath ctx
        TypeFromProperty prop -> prop
        TypeGenerated -> generateFromPath ctx
      
      withPrefix = case conventionAddPrefix naming of
        Just prefix -> prefix <> baseName
        Nothing -> baseName
      
      withoutPrefix = case conventionStripPrefix naming of
        Just prefix -> maybe withPrefix id (T.stripPrefix prefix withPrefix)
        Nothing -> withPrefix
  in sanitizeHaskellIdentifier True withoutPrefix  -- True = capitalize for type
  where
    generateFromPath :: CodegenContext -> Text
    generateFromPath context =
      case codegenParentName context of
        Just parent -> parent <> "Inner"
        Nothing -> "GeneratedType"

-- | Generate a field name from a property name
generateFieldName :: FieldNaming -> Text -> Text
generateFieldName naming propertyName =
  let transformed = case naming of
        FieldUnchanged -> propertyName
        FieldCamelCase -> toCamelCase propertyName
        FieldPascalCase -> toPascalCase propertyName
        FieldSnakeCase -> toSnakeCase propertyName
        FieldStripPrefix prefix -> maybe propertyName id (T.stripPrefix prefix propertyName)
  in sanitizeHaskellIdentifier False transformed  -- False = lowercase for field
  where
    toCamelCase :: Text -> Text
    toCamelCase txt =
      let parts = T.split (\c -> c == '_' || c == '-' || c == ' ') txt
      in case parts of
        [] -> txt
        (first:rest) -> T.toLower first <> T.concat (map capitalize rest)
    
    toPascalCase :: Text -> Text
    toPascalCase txt =
      let parts = T.split (\c -> c == '_' || c == '-' || c == ' ') txt
      in T.concat (map capitalize parts)
    
    toSnakeCase :: Text -> Text
    toSnakeCase = T.toLower . T.map (\c -> if c == '-' || c == ' ' then '_' else c)
    
    capitalize :: Text -> Text
    capitalize txt = case T.uncons txt of
      Nothing -> txt
      Just (c, rest) -> T.cons (toUpper c) rest

-- | Sanitize a name to be a valid Haskell identifier
-- Always returns a non-empty valid identifier
-- Supports Unicode letters/numbers per Haskell spec
sanitizeHaskellIdentifier :: Bool -> Text -> Text
sanitizeHaskellIdentifier shouldCapitalize name =
  let -- Remove invalid characters (keep Unicode letters, numbers, underscore, apostrophe)
      cleaned = T.filter isValidChar name
      -- If nothing remains, use a default name
      nonEmpty = if T.null cleaned then "field" else cleaned
      -- Ensure it doesn't start with a digit or apostrophe
      withValidStart = 
        let first = T.head nonEmpty
        in if isNumber first || first == '\''
           then "x" <> nonEmpty
           else nonEmpty
      -- Apply capitalization
      result = if shouldCapitalize
               then case T.uncons withValidStart of
                 Just (c, rest) -> T.cons (toUpper c) rest
                 Nothing -> "Field"  -- Fallback for empty
               else case T.uncons withValidStart of
                 Just (c, rest) -> T.cons (toLower c) rest
                 Nothing -> "field"  -- Fallback for empty
  in result
  where
    -- Haskell allows Unicode letters, digits, underscore, and apostrophe in identifiers
    isValidChar c = isLetter c || isNumber c || c == '_' || c == '\''

-- | Check if a name is a valid Haskell identifier
-- Supports Unicode letters/numbers per Haskell spec
isValidHaskellIdentifier :: Text -> Bool
isValidHaskellIdentifier name =
  not (T.null name) &&
  isValidStart (T.head name) &&
  T.all isValidChar name
  where
    -- Must start with letter or underscore (not digit or apostrophe)
    isValidStart c = isLetter c || c == '_'
    -- Can contain letters, digits, underscore, and apostrophe
    isValidChar c = isLetter c || isNumber c || c == '_' || c == '\''
