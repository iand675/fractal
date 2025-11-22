{-# LANGUAGE OverloadedStrings #-}
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
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition, KeywordNavigation(..), KeywordScope(..))
import Fractal.JsonSchema.Types 
  ( Schema, SchemaCore(..), SchemaObject(..), ArrayItemsValidation(..), Regex(..)
  , schemaCore, schemaValidation, schemaDefs, SchemaValidation(..)
  , validationProperties, validationPatternProperties, validationAdditionalProperties
  , validationItems, validationPrefixItems, validationContains
  , validationDependentSchemas, validationPropertyNames, validationUnevaluatedProperties
  )

-- | Navigate into 'properties' keyword (SchemaMap)
propertiesKeyword :: KeywordDefinition
propertiesKeyword = mkNavigableKeyword
  "properties"
  AnyScope
  (\_ _ _ -> Right ())  -- No compilation needed for navigation-only
  (\_ _ _ _ -> [])      -- No validation needed for navigation-only
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationProperties (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'patternProperties' keyword (SchemaMap with regex keys)
patternPropertiesKeyword :: KeywordDefinition
patternPropertiesKeyword = mkNavigableKeyword
  "patternProperties"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
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
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationAdditionalProperties (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'items' keyword (can be SingleSchema or SchemaArray in Draft-04)
itemsKeyword :: KeywordDefinition
itemsKeyword = mkNavigableKeyword
  "items"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
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
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> case validationPrefixItems (schemaValidation obj) of
      Just prefixSchemas -> Just (NE.toList prefixSchemas)
      Nothing -> Nothing
    _ -> Nothing)

-- | Navigate into 'contains' keyword (SingleSchema)
containsKeyword :: KeywordDefinition
containsKeyword = mkNavigableKeyword
  "contains"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationContains (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'allOf' keyword (SchemaArray)
allOfKeyword :: KeywordDefinition
allOfKeyword = mkNavigableKeyword
  "allOf"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaAllOf obj)
    _ -> Nothing)

-- | Navigate into 'anyOf' keyword (SchemaArray)
anyOfKeyword :: KeywordDefinition
anyOfKeyword = mkNavigableKeyword
  "anyOf"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaAnyOf obj)
    _ -> Nothing)

-- | Navigate into 'oneOf' keyword (SchemaArray)
oneOfKeyword :: KeywordDefinition
oneOfKeyword = mkNavigableKeyword
  "oneOf"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaArray $ \schema -> case schemaCore schema of
    ObjectSchema obj -> fmap NE.toList (schemaOneOf obj)
    _ -> Nothing)

-- | Navigate into 'not' keyword (SingleSchema)
notKeyword :: KeywordDefinition
notKeyword = mkNavigableKeyword
  "not"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaNot obj
    _ -> Nothing)

-- | Navigate into 'if' keyword (SingleSchema)
ifKeyword :: KeywordDefinition
ifKeyword = mkNavigableKeyword
  "if"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaIf obj
    _ -> Nothing)

-- | Navigate into 'then' keyword (SingleSchema)
thenKeyword :: KeywordDefinition
thenKeyword = mkNavigableKeyword
  "then"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaThen obj
    _ -> Nothing)

-- | Navigate into 'else' keyword (SingleSchema)
elseKeyword :: KeywordDefinition
elseKeyword = mkNavigableKeyword
  "else"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> schemaElse obj
    _ -> Nothing)

-- | Navigate into 'dependentSchemas' keyword (SchemaMap)
dependentSchemasKeyword :: KeywordDefinition
dependentSchemasKeyword = mkNavigableKeyword
  "dependentSchemas"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationDependentSchemas (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'propertyNames' keyword (SingleSchema)
propertyNamesKeyword :: KeywordDefinition
propertyNamesKeyword = mkNavigableKeyword
  "propertyNames"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationPropertyNames (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into 'unevaluatedProperties' keyword (SingleSchema)
unevaluatedPropertiesKeyword :: KeywordDefinition
unevaluatedPropertiesKeyword = mkNavigableKeyword
  "unevaluatedProperties"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SingleSchema $ \schema -> case schemaCore schema of
    ObjectSchema obj -> validationUnevaluatedProperties (schemaValidation obj)
    _ -> Nothing)

-- | Navigate into '$defs' or 'definitions' keyword (SchemaMap)
defsKeyword :: KeywordDefinition
defsKeyword = mkNavigableKeyword
  "$defs"
  AnyScope
  (\_ _ _ -> Right ())
  (\_ _ _ _ -> [])
  (SchemaMap $ \schema -> case schemaCore schema of
    ObjectSchema obj ->
      let defs = schemaDefs obj
      in if Map.null defs then Nothing else Just defs
    _ -> Nothing)

