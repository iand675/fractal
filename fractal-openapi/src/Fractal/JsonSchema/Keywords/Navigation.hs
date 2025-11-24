{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Keyword definitions that provide navigation support for schema resolution
--
-- These keyword definitions are primarily for JSON Pointer resolution ($ref)
-- rather than validation. They declare how keywords contain and expose subschemas.
module Fractal.JsonSchema.Keywords.Navigation
  ( propertiesKeyword
  , patternPropertiesKeyword
  , additionalPropertiesKeyword
  , itemsKeyword
  , prefixItemsKeyword
  , containsKeyword
  , allOfKeyword
  , anyOfKeyword
  , oneOfKeyword
  , notKeyword
  , ifKeyword
  , thenKeyword
  , elseKeyword
  , dependentSchemasKeyword
  , propertyNamesKeyword
  , unevaluatedPropertiesKeyword
  , defsKeyword
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Text.Read as Read

import Fractal.JsonSchema.Keyword (mkNavigableKeyword)
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition, KeywordNavigation(..)
  , CompilationContext(..), ValidateFunc, CompileFunc
  )
import Fractal.JsonSchema.Types 
  ( Schema, SchemaCore(..), SchemaObject(..), ArrayItemsValidation(..), Regex(..)
  , schemaCore, schemaValidation, schemaDefs, SchemaValidation(..)
  , validationProperties, validationPatternProperties, validationAdditionalProperties
  , validationItems, validationPrefixItems, validationContains
  , validationDependentSchemas, validationPropertyNames, validationUnevaluatedProperties
  , ValidationAnnotations(..), pattern ValidationSuccess, pattern ValidationFailure
  , ValidationResult, ValidationErrors(..), emptyPointer
  , JsonSchemaVersion(..), schemaVersion
  )
import qualified Fractal.JsonSchema.Validator.Result as VR
import Fractal.JsonSchema.Parser.Internal (parseSchemaValue)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value(..), object)
import Control.Monad.Reader (ask)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

-- | Navigate into 'properties' keyword (SchemaMap)
propertiesKeyword :: KeywordDefinition
propertiesKeyword = mkNavigableKeyword
  "properties"
  (\_ _ _ -> Right ())  -- No compilation needed for navigation-only
  (\_ _ _ _ -> pure (ValidationSuccess mempty))      -- No validation needed for navigation-only
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationProperties (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'patternProperties' keyword (SchemaMap with regex keys)
patternPropertiesKeyword :: KeywordDefinition
patternPropertiesKeyword = mkNavigableKeyword
  "patternProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj ->
      case validationPatternProperties (schemaValidation obj) of
        Just patterns ->
          -- Convert Regex keys to Text keys for navigation
          Just $ Map.fromList [(pat, schema) | (Regex pat, schema) <- Map.toList patterns]
        Nothing -> Nothing
    _ -> Nothing)

-- | Navigate into 'additionalProperties' keyword (SingleSchema)
additionalPropertiesKeyword :: KeywordDefinition
additionalPropertiesKeyword = mkNavigableKeyword
  "additionalProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationAdditionalProperties (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'items' keyword (can be SingleSchema or SchemaArray in Draft-04)
itemsKeyword :: KeywordDefinition
itemsKeyword = mkNavigableKeyword
  "items"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (CustomNavigation $ \schema _seg rest -> case schemaCore schema of
    ObjectSchema obj ->
      case validationItems (schemaValidation obj) of
        Just (ItemsSchema itemSchema) ->
          -- Single schema for all items - return it with full rest path
          Just (itemSchema, rest)
        Just (ItemsTuple schemas _) ->
          -- Array of schemas - need index from rest
          case rest of
            (idx:remaining) ->
              case reads (T.unpack idx) :: [(Int, String)] of
                [(n, "")] | n >= 0 && n < length schemas ->
                  Just (NE.toList schemas !! n, remaining)
                _ -> Nothing
            [] -> Nothing  -- Need an index for tuple items
        Nothing -> Nothing
    _ -> Nothing)

-- | Navigate into 'prefixItems' keyword (SchemaArray)
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = mkNavigableKeyword
  "prefixItems"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> case validationPrefixItems (schemaValidation obj) of
      Just prefixSchemas -> Just (NE.toList prefixSchemas)
      Nothing -> Nothing
    _ -> Nothing)

-- | Navigate into 'contains' keyword (SingleSchema)
containsKeyword :: KeywordDefinition
containsKeyword = mkNavigableKeyword
  "contains"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationContains (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'allOf' keyword (SchemaArray)
allOfKeyword :: KeywordDefinition
allOfKeyword = mkNavigableKeyword
  "allOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaAllOf obj)
    _ -> Nothing)

-- | Navigate into 'anyOf' keyword (SchemaArray)
anyOfKeyword :: KeywordDefinition
anyOfKeyword = mkNavigableKeyword
  "anyOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaAnyOf obj)
    _ -> Nothing)

-- | Navigate into 'oneOf' keyword (SchemaArray)
oneOfKeyword :: KeywordDefinition
oneOfKeyword = mkNavigableKeyword
  "oneOf"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaOneOf obj)
    _ -> Nothing)

-- | Navigate into 'not' keyword (SingleSchema)
notKeyword :: KeywordDefinition
notKeyword = mkNavigableKeyword
  "not"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaNot obj
    _ -> Nothing)

-- | Navigate into 'if' keyword (SingleSchema)
ifKeyword :: KeywordDefinition
ifKeyword = mkNavigableKeyword
  "if"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaIf obj
    _ -> Nothing)

-- | Navigate into 'then' keyword (SingleSchema)
thenKeyword :: KeywordDefinition
thenKeyword = mkNavigableKeyword
  "then"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaThen obj
    _ -> Nothing)

-- | Navigate into 'else' keyword (SingleSchema)
elseKeyword :: KeywordDefinition
elseKeyword = mkNavigableKeyword
  "else"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaElse obj
    _ -> Nothing)

-- | Navigate into 'dependentSchemas' keyword (SchemaMap)
dependentSchemasKeyword :: KeywordDefinition
dependentSchemasKeyword = mkNavigableKeyword
  "dependentSchemas"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationDependentSchemas (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'propertyNames' keyword (SingleSchema)
propertyNamesKeyword :: KeywordDefinition
propertyNamesKeyword = mkNavigableKeyword
  "propertyNames"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationPropertyNames (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'unevaluatedProperties' keyword (SingleSchema)
unevaluatedPropertiesKeyword :: KeywordDefinition
unevaluatedPropertiesKeyword = mkNavigableKeyword
  "unevaluatedProperties"
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> pure (ValidationSuccess mempty))
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationUnevaluatedProperties (schemaValidation obj)
    _ -> Nothing)

-- | Compiled data for $defs keyword
-- Stores the schema that validates each definition value (from metaschema's additionalProperties)
newtype DefsData = DefsData Schema
  deriving (Typeable)

-- | Compile $defs keyword
-- The metaschema defines $defs with additionalProperties: { "$recursiveRef": "#" }
-- So we need to compile that schema to validate each definition value.
compileDefsKeyword :: CompileFunc DefsData
compileDefsKeyword (Object _defsObj) schema ctx = do
  -- The metaschema defines $defs with additionalProperties: { "$recursiveRef": "#" }
  -- We need to create a schema that validates each value in $defs against the metaschema.
  -- Since we're compiling $defs from the schema, we need to get the metaschema's definition.
  -- For now, we'll create a schema with $recursiveRef: "#" which will resolve to the metaschema.
  -- But we need the current schema version to parse the $recursiveRef correctly.
  let version = fromMaybe Draft202012 (schemaVersion schema)
      -- Create a schema that validates each definition value against the metaschema
      -- For 2019-09: additionalProperties: { "$recursiveRef": "#" }
      -- For 2020-12: additionalProperties: { "$dynamicRef": "#meta" }
      refValue = case version of
        Draft201909 -> object [("$recursiveRef", String "#")]
        _ -> object [("$dynamicRef", String "#meta")]  -- 2020-12+
  case parseSchemaValue version refValue of
    Left err -> Left $ "Failed to compile $defs validation schema: " <> T.pack (show err)
    Right defsValidationSchema -> Right $ DefsData defsValidationSchema
compileDefsKeyword _ _ _ = Left "$defs must be an object"

-- | Validate $defs keyword
-- When validating instance data against a schema that references the metaschema,
-- each value in the instance's $defs must be a valid schema (validated against the metaschema).
validateDefsKeyword :: ValidateFunc DefsData
validateDefsKeyword recursiveValidator (DefsData defsValidationSchema) _ctx (Object objMap) = do
  -- Check for $defs or definitions in the instance
  let defsValue = KeyMap.lookup (Key.fromText "$defs") objMap
      defsValue' = case defsValue of
        Just v -> Just v
        Nothing -> KeyMap.lookup (Key.fromText "definitions") objMap
  case defsValue' of
    Just (Object defsObj) ->
      -- Validate each value in $defs against the validation schema
      -- The validation schema has $recursiveRef: "#" which resolves to the metaschema
      let results = 
            [ recursiveValidator defsValidationSchema defValue
            | (defName, defValue) <- KeyMap.toList defsObj
            ]
          failures = [errs | ValidationFailure errs <- results]
      in pure $ case failures of
        [] -> ValidationSuccess mempty
        (e:es) -> ValidationFailure $ foldl (<>) e es
    Just _ -> 
      -- $defs is not an object
      pure $ ValidationFailure $ ValidationErrors $ pure $ VR.ValidationError
        { VR.errorKeyword = "$defs"
        , VR.errorSchemaPath = emptyPointer
        , VR.errorInstancePath = emptyPointer
        , VR.errorMessage = "$defs must be an object"
        }
    Nothing -> 
      -- No $defs in instance, that's fine
      pure $ ValidationSuccess mempty
validateDefsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to objects

-- | Navigate into '$defs' or 'definitions' keyword (SchemaMap)
defsKeyword :: KeywordDefinition
defsKeyword = mkNavigableKeyword
  "$defs"
  compileDefsKeyword
  validateDefsKeyword
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj ->
      let defs = schemaDefs obj
      in if Map.null defs then Nothing else Just defs
    _ -> Nothing)

