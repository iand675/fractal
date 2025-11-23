-- | JSON Schema parsing with automatic version detection
--
-- This module provides functions to parse JSON Schema from JSON/YAML values.
-- The parser supports all versions (draft-04 through 2020-12) with automatic
-- version detection from the $schema keyword.
module Fractal.JsonSchema.Parser
  ( -- * Parsing Functions
    parseSchema
  , parseSchemaWithVersion
  , parseSubschema
  , parseSchemaStrict
  , parseSchemaFromFile
  
    -- * Dialect-Aware Parsing
  , parseSchemaWithDialectRegistry
  , parseSchemaWithDialectRegistryAndVersion
  , resolveDialectFromSchema
  , extractSchemaURI

    -- * Error Types
  , ParseError(..)
  ) where

import Fractal.JsonSchema.Types
import Fractal.JsonSchema.Vocabulary (VocabularyRegistry, lookupDialect)
import Fractal.JsonSchema.Dialect (Dialect, dialectURI, dialectVersion)
import Data.Aeson (Value(..), Object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Foldable (toList)
import Control.Monad (when)

-- | Error during schema parsing
data ParseError = ParseError
  { parseErrorPath :: JSONPointer      -- ^ Where in the schema
  , parseErrorMessage :: Text           -- ^ What went wrong
  , parseErrorContext :: Maybe Value    -- ^ Problematic value
  } deriving (Eq, Show)

-- | Parse a JSON Schema from a Value with automatic version detection
parseSchema :: Value -> Either ParseError Schema
parseSchema val = do
  version <- detectVersion val
  parseSchemaWithVersion version val

-- | Parse a subschema, inheriting the parent version unless the subschema
--   declares its own $schema.
parseSubschema :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSubschema parentVersion val = case val of
  Object obj
    | KeyMap.member "$schema" obj -> parseSchema val
    | otherwise -> parseSchemaWithVersion parentVersion val
  _ -> parseSchemaWithVersion parentVersion val

-- | Parse with explicit version (skip detection)
parseSchemaWithVersion :: JsonSchemaVersion -> Value -> Either ParseError Schema
parseSchemaWithVersion version val = case val of
  Bool b -> Right $ Schema
    { schemaVersion = Just version
    , schemaMetaschemaURI = Nothing  -- Boolean schemas don't have $schema
    , schemaId = Nothing
    , schemaCore = BooleanSchema b
    , schemaVocabulary = Nothing
    , schemaExtensions = Map.empty
    , schemaRawKeywords = Map.empty  -- Boolean schemas have no keywords
    }
  Object obj -> do
    -- Parse $schema URI
    let metaschemaURI = KeyMap.lookup "$schema" obj >>= \case
          String t -> Just t
          _ -> Nothing

    -- Parse $id (draft-06+) or id (draft-04 only)
    let idKey = if version >= Draft06 then "$id" else "id"
    let schemaId' = KeyMap.lookup idKey obj >>= \case
          String t -> Just t
          _ -> Nothing

    -- Parse $vocabulary (2019-09+)
    let vocabMap = if version >= Draft201909
                   then KeyMap.lookup "$vocabulary" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                   else Nothing

    -- Parse core structure
    core <- parseSchemaObject version obj

    -- Collect unknown keywords (extensions)
    -- TODO (US3): This is where vocabulary integration happens
    -- For each unknown keyword:
    --   1. Check if vocabulary is registered
    --   2. Use keywordParser from KeywordDefinition to get typed KeywordValue
    --   3. Store in schemaCustomKeywords :: Map Text KeywordValue
    --   4. Truly unknown keywords stay in schemaExtensions
    let knownKeywords = Set.fromList
          [ "$schema", "$id", "id", "$ref", "$vocabulary", "$defs", "definitions"
          , "$comment", "$anchor", "$dynamicRef", "$dynamicAnchor"
          , "type", "enum", "const"
          , "allOf", "anyOf", "oneOf", "not"
          , "if", "then", "else"
          , "title", "description", "default"
          , "deprecated", "readOnly", "writeOnly"
          , "properties", "patternProperties", "additionalProperties"
          , "propertyNames", "required", "minProperties", "maxProperties"
          , "dependentRequired", "dependentSchemas", "dependencies"
          , "unevaluatedProperties"
          , "items", "prefixItems", "additionalItems", "contains"
          , "minItems", "maxItems", "uniqueItems"
          , "maxContains", "minContains", "unevaluatedItems"
          , "minLength", "maxLength", "pattern", "format"
          , "contentEncoding", "contentMediaType"
          , "minimum", "maximum", "exclusiveMinimum", "exclusiveMaximum"
          , "multipleOf"
          ]
    let allKeywords = Map.fromList [(Key.toText k, v) | (k, v) <- KeyMap.toList obj]
    let extensions = Map.filterWithKey (\k _ -> not $ Set.member k knownKeywords) allKeywords

    pure $ Schema
      { schemaVersion = Just version
      , schemaMetaschemaURI = metaschemaURI
      , schemaId = schemaId'
      , schemaCore = ObjectSchema core
      , schemaVocabulary = vocabMap
      , schemaExtensions = extensions
      , schemaRawKeywords = allKeywords  -- Store ALL keywords for monadic compilation
      }
  _ -> Left $ ParseError emptyPointer "Schema must be boolean or object" (Just val)

-- | Strict parsing (fail on unknown keywords)
parseSchemaStrict :: Value -> Either ParseError Schema
parseSchemaStrict = parseSchema  -- TODO: Implement strict mode

-- | Parse from file (handles JSON and YAML)
parseSchemaFromFile :: FilePath -> IO (Either ParseError Schema)
parseSchemaFromFile = error "Not yet implemented"  -- TODO: Implement file loading

-- | Detect schema version from $schema keyword
detectVersion :: Value -> Either ParseError JsonSchemaVersion
detectVersion (Object obj) = case KeyMap.lookup "$schema" obj of
  Nothing -> pure Draft202012  -- Default to latest
  Just uri -> case Aeson.fromJSON uri of
    Aeson.Success ver -> pure ver
    Aeson.Error err -> Left $ ParseError emptyPointer (T.pack err) Nothing
detectVersion _ = pure Draft202012  -- Boolean schemas default to latest

-- | Parse full schema object
parseSchemaObject :: JsonSchemaVersion -> Object -> Either ParseError SchemaObject
parseSchemaObject version obj = do
  -- Parse type keyword
  let schemaType' = KeyMap.lookup "type" obj >>= \v -> case v of
        String t -> case parseSchemaType t of
          Just st -> Just (One st)
          Nothing -> Nothing
        Array arr -> case traverse (AesonTypes.parseEither Aeson.parseJSON) (toList arr) of
          Right types -> NE.nonEmpty types >>= Just . Many
          Left _ -> Nothing
        _ -> Nothing
  
  -- Parse enum
  let schemaEnum' = KeyMap.lookup "enum" obj >>= \v -> case v of
        Array arr -> NE.nonEmpty (toList arr)
        _ -> Nothing
  
  -- Parse const (draft-06+)
  let schemaConst' = if version >= Draft06
                     then KeyMap.lookup "const" obj
                     else Nothing
  
  -- Parse $ref
  let schemaRef' = KeyMap.lookup "$ref" obj >>= \v -> case v of
        String t -> Just (Reference t)
        _ -> Nothing
  
  -- Parse composition keywords
  let parseNonEmptySchemas key = KeyMap.lookup key obj >>= \v -> case v of
        Array arr -> do
          let schemas = [schema | val <- toList arr, Right schema <- [parseSubschema version val]]
          NE.nonEmpty schemas
        _ -> Nothing
  
  let schemaAllOf' = parseNonEmptySchemas "allOf"
  let schemaAnyOf' = parseNonEmptySchemas "anyOf"
  let schemaOneOf' = parseNonEmptySchemas "oneOf"
  
  let schemaNot' = KeyMap.lookup "not" obj >>= eitherToMaybe . parseSubschema version
  
  -- Parse conditional keywords (draft-07+)
  let (schemaIf', schemaThen', schemaElse') = if version >= Draft07
        then ( KeyMap.lookup "if" obj >>= eitherToMaybe . parseSubschema version
             , KeyMap.lookup "then" obj >>= eitherToMaybe . parseSubschema version
             , KeyMap.lookup "else" obj >>= eitherToMaybe . parseSubschema version
             )
        else (Nothing, Nothing, Nothing)
  
  -- Parse validation keywords
  validation <- parseValidationKeywords version obj
  
  -- Parse annotations
  let annotations = SchemaAnnotations
        { annotationTitle = KeyMap.lookup "title" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationDescription = KeyMap.lookup "description" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationDefault = KeyMap.lookup "default" obj
        , annotationExamples = case KeyMap.lookup "examples" obj of
            Just (Array arr) -> toList arr
            _ -> []
        , annotationDeprecated = KeyMap.lookup "deprecated" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationReadOnly = KeyMap.lookup "readOnly" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationWriteOnly = KeyMap.lookup "writeOnly" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
        , annotationComment = if version >= Draft07
                              then KeyMap.lookup "$comment" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                              else Nothing
        , annotationCodegen = Nothing  -- TODO: Parse x-codegen annotations
        }
  
  -- Parse $defs or definitions
  let defsKey = if version >= Draft201909 then "$defs" else "definitions"
  let schemaDefs' = case KeyMap.lookup defsKey obj of
        Just (Object defsObj) -> Map.fromList
          [(Key.toText k, schema) | (k, v) <- KeyMap.toList defsObj
          , Right schema <- [parseSubschema version v]]
        _ -> Map.empty
  
  -- Parse $dynamicRef (2020-12+)
  let dynamicRef' = if version >= Draft202012
                    then KeyMap.lookup "$dynamicRef" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  
  -- Parse $dynamicAnchor (2020-12+)
  let dynamicAnchor' = if version >= Draft202012
                       then KeyMap.lookup "$dynamicAnchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                       else Nothing

  -- Parse $recursiveRef (2019-09)
  let recursiveRef' = if version == Draft201909
                      then KeyMap.lookup "$recursiveRef" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                      else Nothing

  -- Parse $recursiveAnchor (2019-09)
  let recursiveAnchor' = if version == Draft201909
                         then KeyMap.lookup "$recursiveAnchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                         else Nothing

  pure $ SchemaObject
    { schemaType = schemaType'
    , schemaEnum = schemaEnum'
    , schemaConst = schemaConst'
    , schemaRef = schemaRef'
    , schemaDynamicRef = dynamicRef'
    , schemaAnchor = KeyMap.lookup "$anchor" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
    , schemaDynamicAnchor = dynamicAnchor'
    , schemaRecursiveRef = recursiveRef'
    , schemaRecursiveAnchor = recursiveAnchor'
    , schemaAllOf = schemaAllOf'
    , schemaAnyOf = schemaAnyOf'
    , schemaOneOf = schemaOneOf'
    , schemaNot = schemaNot'
    , schemaIf = schemaIf'
    , schemaThen = schemaThen'
    , schemaElse = schemaElse'
    , schemaValidation = validation
    , schemaAnnotations = annotations
    , schemaDefs = schemaDefs'
    }

-- | Helper to convert Either to Maybe
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _) = Nothing

-- | Parse schema type from text
parseSchemaType :: Text -> Maybe SchemaType
parseSchemaType "null" = Just NullType
parseSchemaType "boolean" = Just BooleanType
parseSchemaType "object" = Just ObjectType
parseSchemaType "array" = Just ArrayType
parseSchemaType "number" = Just NumberType
parseSchemaType "string" = Just StringType
parseSchemaType "integer" = Just IntegerType
parseSchemaType _ = Nothing

-- | Parse validation keywords
parseValidationKeywords :: JsonSchemaVersion -> Object -> Either ParseError SchemaValidation
parseValidationKeywords version obj = do
  -- Numeric validation
  let multipleOf = KeyMap.lookup "multipleOf" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let maximum' = KeyMap.lookup "maximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minimum' = KeyMap.lookup "minimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  
  -- exclusiveMaximum/Minimum: boolean in draft-04, numeric in draft-06+
  let (exclMax, exclMin) = if version == Draft04
        then ( KeyMap.lookup "exclusiveMaximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Left
             , KeyMap.lookup "exclusiveMinimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Left
             )
        else ( KeyMap.lookup "exclusiveMaximum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Right
             , KeyMap.lookup "exclusiveMinimum" obj >>= AesonTypes.parseMaybe Aeson.parseJSON >>= Just . Right
             )
  
  -- String validation
  let maxLength = KeyMap.lookup "maxLength" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minLength = KeyMap.lookup "minLength" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let pattern' = KeyMap.lookup "pattern" obj >>= \v -> case v of
        String t -> Just (Regex t)
        _ -> Nothing
  let format' = KeyMap.lookup "format" obj >>= AesonTypes.parseMaybe Aeson.parseJSON

  -- Content validation (draft-07+)
  let contentEncoding' = if version >= Draft07
                         then KeyMap.lookup "contentEncoding" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                         else Nothing
  let contentMediaType' = if version >= Draft07
                          then KeyMap.lookup "contentMediaType" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                          else Nothing

  -- Array validation
  let items' = parseArrayItems version obj
  let prefixItems' = if version >= Draft202012
                     then KeyMap.lookup "prefixItems" obj >>= parseArraySchemas version
                     else Nothing
  let contains' = KeyMap.lookup "contains" obj >>= eitherToMaybe . parseSubschema version
  let maxItems = KeyMap.lookup "maxItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minItems = KeyMap.lookup "minItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let uniqueItems = KeyMap.lookup "uniqueItems" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let maxContains = if version >= Draft201909
                    then KeyMap.lookup "maxContains" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  let minContains = if version >= Draft201909
                    then KeyMap.lookup "minContains" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                    else Nothing
  let unevaluatedItems' = if version >= Draft201909
                          then KeyMap.lookup "unevaluatedItems" obj >>= eitherToMaybe . parseSubschema version
                          else Nothing
  
  -- Object validation
  let properties' = KeyMap.lookup "properties" obj >>= parsePropertySchemas version
  let patternProperties' = KeyMap.lookup "patternProperties" obj >>= parsePatternPropertySchemas version
  let additionalProperties' = KeyMap.lookup "additionalProperties" obj >>= \v -> case v of
        Bool b -> Just $ Schema Nothing Nothing Nothing (BooleanSchema b) Nothing Map.empty Map.empty
        _ -> eitherToMaybe $ parseSubschema version v
  let unevaluatedProperties' = if version >= Draft201909
                               then KeyMap.lookup "unevaluatedProperties" obj >>= eitherToMaybe . parseSubschema version
                               else Nothing
  let propertyNames' = if version >= Draft06
                      then KeyMap.lookup "propertyNames" obj >>= eitherToMaybe . parseSubschema version
                       else Nothing
  let maxProperties = KeyMap.lookup "maxProperties" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let minProperties = KeyMap.lookup "minProperties" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
  let required' = KeyMap.lookup "required" obj >>= \v -> case v of
        Array arr -> Just $ Set.fromList [t | String t <- toList arr]
        _ -> Nothing
  let dependentRequired' = if version >= Draft201909
                           then KeyMap.lookup "dependentRequired" obj >>= AesonTypes.parseMaybe Aeson.parseJSON
                           else Nothing
  let dependentSchemas' = if version >= Draft201909
                          then KeyMap.lookup "dependentSchemas" obj >>= parsePropertySchemas version
                          else Nothing
  -- Dependencies keyword: deprecated in 2019-09+ but still supported for backward compatibility
  let dependencies' = KeyMap.lookup "dependencies" obj >>= parseDependencies version
  
  pure $ SchemaValidation
    { validationMultipleOf = multipleOf
    , validationMaximum = maximum'
    , validationExclusiveMaximum = exclMax
    , validationMinimum = minimum'
    , validationExclusiveMinimum = exclMin
    , validationMaxLength = maxLength
    , validationMinLength = minLength
    , validationPattern = pattern'
    , validationFormat = format'
    , validationContentEncoding = contentEncoding'
    , validationContentMediaType = contentMediaType'
    , validationItems = items'
    , validationPrefixItems = prefixItems'
    , validationContains = contains'
    , validationMaxItems = maxItems
    , validationMinItems = minItems
    , validationUniqueItems = uniqueItems
    , validationMaxContains = maxContains
    , validationMinContains = minContains
    , validationUnevaluatedItems = unevaluatedItems'
    , validationProperties = properties'
    , validationPatternProperties = patternProperties'
    , validationAdditionalProperties = additionalProperties'
    , validationUnevaluatedProperties = unevaluatedProperties'
    , validationPropertyNames = propertyNames'
    , validationMaxProperties = maxProperties
    , validationMinProperties = minProperties
    , validationRequired = required'
    , validationDependentRequired = dependentRequired'
    , validationDependentSchemas = dependentSchemas'
    , validationDependencies = dependencies'
    }

parseArrayItems :: JsonSchemaVersion -> Object -> Maybe ArrayItemsValidation
parseArrayItems version obj = do
  itemsVal <- KeyMap.lookup "items" obj
  case itemsVal of
    Array arr -> do
      -- Tuple-style items (array of schemas)
      let schemas = [schema | val <- toList arr, Right schema <- [parseSubschema version val]]
      nonEmpty <- NE.nonEmpty schemas
      -- Parse additionalItems if present
      let additionalItems' = KeyMap.lookup "additionalItems" obj >>= eitherToMaybe . parseSubschema version
      pure $ ItemsTuple nonEmpty additionalItems'
    _ -> do
      -- Single schema for all items
      schema <- eitherToMaybe (parseSubschema version itemsVal)
      pure $ ItemsSchema schema

-- | Parse non-empty array of schemas
parseArraySchemas :: JsonSchemaVersion -> Value -> Maybe (NonEmpty Schema)
parseArraySchemas version (Array arr) = do
  let schemas = [schema | val <- toList arr, Right schema <- [parseSubschema version val]]
  NE.nonEmpty schemas
parseArraySchemas _ _ = Nothing

-- | Parse property schemas map
parsePropertySchemas :: JsonSchemaVersion -> Value -> Maybe (Map Text Schema)
parsePropertySchemas version (Object obj) = Just $ Map.fromList
  [(Key.toText k, schema) | (k, v) <- KeyMap.toList obj
  , Right schema <- [parseSubschema version v]]
parsePropertySchemas _ _ = Nothing

-- | Parse pattern property schemas
parsePatternPropertySchemas :: JsonSchemaVersion -> Value -> Maybe (Map Regex Schema)
parsePatternPropertySchemas version (Object obj) = Just $ Map.fromList
  [(Regex (Key.toText k), schema) | (k, v) <- KeyMap.toList obj
  , Right schema <- [parseSubschema version v]]
parsePatternPropertySchemas _ _ = Nothing

--- | Parse dependencies map (draft-04 through draft-07)
parseDependencies :: JsonSchemaVersion -> Value -> Maybe (Map Text Dependency)
parseDependencies version (Object obj) = Just $ Map.fromList
  [(Key.toText k, dep) | (k, v) <- KeyMap.toList obj
  , Just dep <- [parseDependency version v]]
parseDependencies _ _ = Nothing

--- | Parse a single dependency
parseDependency :: JsonSchemaVersion -> Value -> Maybe Dependency
parseDependency version v@(Object _) =
  case parseSubschema version v of
    Right schema -> Just $ DependencySchema schema
    Left _ -> Nothing
parseDependency version v@(Bool _) =
  -- Boolean schemas (true/false) are valid dependencies
  case parseSubschema version v of
    Right schema -> Just $ DependencySchema schema
    Left _ -> Nothing
parseDependency _ (Array arr) = Just $ DependencyProperties $ Set.fromList
  [t | String t <- toList arr]
parseDependency _ _ = Nothing

-- | Parse a schema with dialect registry support
--
-- This function parses a JSON Schema and uses the provided dialect registry
-- to look up the dialect based on the $schema keyword.
--
-- Behavior:
-- - If $schema is present and found in registry: Use that dialect
-- - If $schema is present but NOT in registry: Error (even for standard URIs)
-- - If no $schema: Use default parsing (Draft 2020-12)
--
-- When using a dialect registry, ALL dialects must be explicitly registered.
-- Use 'standardDialectRegistry' or 'standardRegistry' to get standard dialects.
-- This ensures explicit control over which dialects are accepted.
parseSchemaWithDialectRegistry
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> Value               -- ^ JSON value to parse
  -> Either ParseError Schema
parseSchemaWithDialectRegistry registry val = do
  -- Extract $schema URI if present
  let mSchemaURI = extractSchemaURI val
  
  -- Try to resolve dialect from registry
  case mSchemaURI of
    Just uri -> case lookupDialect uri registry of
      Just dialect -> do
        -- Found dialect - parse with its version
        parseSchemaWithVersion (dialectVersion dialect) val
      Nothing -> 
        -- Dialect not in registry - error
        Left $ ParseError
          { parseErrorPath = emptyPointer
          , parseErrorMessage = "Unregistered dialect: " <> uri <> ". " <> 
                               "Dialect must be registered in the dialect registry. " <>
                               "Use standardDialectRegistry to include standard JSON Schema dialects."
          , parseErrorContext = Nothing
          }
    Nothing -> do
      -- No $schema - use default parsing
      parseSchema val

-- | Parse a schema with explicit version and dialect registry
--
-- This variant allows overriding the version while still looking up
-- dialect information for validation configuration.
parseSchemaWithDialectRegistryAndVersion
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> JsonSchemaVersion   -- ^ Explicit version to use
  -> Value               -- ^ JSON value to parse
  -> Either ParseError Schema
parseSchemaWithDialectRegistryAndVersion registry version val = do
  parseSchemaWithVersion version val

-- | Resolve the dialect for a parsed schema
--
-- Given a schema and a dialect registry, this function looks up the dialect
-- based on the schemaMetaschemaURI. Returns Nothing if:
-- - The schema has no $schema declaration
-- - The dialect is not found in the registry
--
-- This is useful during validation to access dialect-specific configuration.
resolveDialectFromSchema
  :: VocabularyRegistry  -- ^ Registry containing available dialects
  -> Schema              -- ^ Parsed schema
  -> Maybe Dialect
resolveDialectFromSchema registry schema =
  schemaMetaschemaURI schema >>= \uri -> lookupDialect uri registry

-- | Extract $schema URI from a JSON value
extractSchemaURI :: Value -> Maybe Text
extractSchemaURI (Object obj) = case KeyMap.lookup "$schema" obj of
  Just (String uri) -> Just uri
  _ -> Nothing
extractSchemaURI _ = Nothing

