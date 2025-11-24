{-# LANGUAGE OverloadedStrings #-}
-- | Standard JSON Schema keywords implemented using the pluggable keyword system
--
-- This module re-exports all standard JSON Schema keywords from their
-- individual modules. These keywords are no longer privileged - they're
-- implemented exactly like custom keywords would be.
module Fractal.JsonSchema.Keywords.Standard
  ( -- * Basic Keywords
    constKeyword
  , enumKeyword
  , typeKeyword
    -- * String Keywords
  , minLengthKeyword
  , maxLengthKeyword
  , patternKeyword
    -- * Numeric Keywords
  , minimumKeyword
  , maximumKeyword
  , multipleOfKeyword
  , exclusiveMinimumKeyword
  , exclusiveMaximumKeyword
    -- * Array Keywords
  , minItemsKeyword
  , maxItemsKeyword
  , uniqueItemsKeyword
  , itemsKeyword
  , prefixItemsKeyword
  , containsKeyword
  , minContainsKeyword
  , maxContainsKeyword
    -- * Object Keywords
  , requiredKeyword
  , minPropertiesKeyword
  , maxPropertiesKeyword
  , propertiesKeyword
  , patternPropertiesKeyword
  , additionalPropertiesKeyword
  , propertyNamesKeyword
  , dependentRequiredKeyword
  , dependentSchemasKeyword
  , unevaluatedPropertiesKeyword
  , unevaluatedItemsKeyword
  , formatKeyword
    -- * Conditional Keywords
  , ifKeyword
  , thenKeyword
  , elseKeyword
    -- * Registry
  , draft202012Registry
  , draft201909Registry
  , draft07Registry
  , draft06Registry
  , draft04Registry
  , standardKeywordRegistry
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Fractal.JsonSchema.Keyword (KeywordRegistry(..), emptyKeywordRegistry, registerKeyword)
-- Basic keywords
import Fractal.JsonSchema.Keywords.Const (constKeyword)
import Fractal.JsonSchema.Keywords.Enum (enumKeyword)
import Fractal.JsonSchema.Keywords.Type (typeKeyword)
-- String keywords
import Fractal.JsonSchema.Keywords.MinLength (minLengthKeyword)
import Fractal.JsonSchema.Keywords.MaxLength (maxLengthKeyword)
import Fractal.JsonSchema.Keywords.Pattern (patternKeyword)
-- Numeric keywords
import Fractal.JsonSchema.Keywords.Minimum (minimumKeyword)
import Fractal.JsonSchema.Keywords.Maximum (maximumKeyword)
import Fractal.JsonSchema.Keywords.MultipleOf (multipleOfKeyword)
import Fractal.JsonSchema.Keywords.ExclusiveMinimum (exclusiveMinimumKeyword)
import Fractal.JsonSchema.Keywords.ExclusiveMaximum (exclusiveMaximumKeyword)
-- Array keywords
import Fractal.JsonSchema.Keywords.MinItems (minItemsKeyword)
import Fractal.JsonSchema.Keywords.MaxItems (maxItemsKeyword)
import Fractal.JsonSchema.Keywords.UniqueItems (uniqueItemsKeyword)
import Fractal.JsonSchema.Keywords.Items (itemsKeyword)
import Fractal.JsonSchema.Keywords.PrefixItems (prefixItemsKeyword)
import Fractal.JsonSchema.Keywords.Contains (containsKeyword, minContainsKeyword, maxContainsKeyword)
-- Object keywords
import Fractal.JsonSchema.Keywords.Required (requiredKeyword)
import Fractal.JsonSchema.Keywords.MinProperties (minPropertiesKeyword)
import Fractal.JsonSchema.Keywords.MaxProperties (maxPropertiesKeyword)
import Fractal.JsonSchema.Keywords.Properties (propertiesKeyword)
import Fractal.JsonSchema.Keywords.PatternProperties (patternPropertiesKeyword)
import Fractal.JsonSchema.Keywords.AdditionalProperties (additionalPropertiesKeyword)
import Fractal.JsonSchema.Keywords.PropertyNames (propertyNamesKeyword)
import Fractal.JsonSchema.Keywords.Dependencies (dependenciesKeyword)
import Fractal.JsonSchema.Keywords.DependentRequired (dependentRequiredKeyword)
import Fractal.JsonSchema.Keywords.DependentSchemas (dependentSchemasKeyword)
import Fractal.JsonSchema.Keywords.UnevaluatedProperties (unevaluatedPropertiesKeyword)
import Fractal.JsonSchema.Keywords.UnevaluatedItems (unevaluatedItemsKeyword)
import Fractal.JsonSchema.Keywords.FormatKeyword (formatKeyword)
import Fractal.JsonSchema.Keywords.ContentMediaType (contentMediaTypeKeyword)
import Fractal.JsonSchema.Keywords.ContentEncoding (contentEncodingKeyword)

-- Conditional keywords
import Fractal.JsonSchema.Keywords.Conditional (ifKeyword, thenKeyword, elseKeyword)
-- Navigable keywords (for $ref resolution)
import qualified Fractal.JsonSchema.Keywords.Navigation as Nav
import qualified Fractal.JsonSchema.Keywords.AllOf as AllOf
import qualified Fractal.JsonSchema.Keywords.AnyOf as AnyOf
import qualified Fractal.JsonSchema.Keywords.OneOf as OneOf
import qualified Fractal.JsonSchema.Keywords.Not as Not
-- Draft-04 specific keywords
import qualified Fractal.JsonSchema.Keywords.Draft04.Minimum as D04
import qualified Fractal.JsonSchema.Keywords.Draft04.Maximum as D04

-- | Registry containing all keywords defined in Draft 2020-12
draft202012Registry :: KeywordRegistry
draft202012Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword prefixItemsKeyword $
  registerKeyword containsKeyword $
  registerKeyword minContainsKeyword $
  registerKeyword maxContainsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  registerKeyword dependentRequiredKeyword $
  registerKeyword dependentSchemasKeyword $
  registerKeyword unevaluatedPropertiesKeyword $
  registerKeyword unevaluatedItemsKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft 2019-09
-- 
-- Draft 2019-09 adds:
-- - dependentSchemas/dependentRequired (replaces dependencies)
-- - unevaluatedProperties/unevaluatedItems
-- - minContains/maxContains
-- - $recursiveRef/$recursiveAnchor
-- 
-- Does NOT include prefixItems (added in 2020-12)
draft201909Registry :: KeywordRegistry
draft201909Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  registerKeyword minContainsKeyword $
  registerKeyword maxContainsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  registerKeyword dependentRequiredKeyword $
  registerKeyword dependentSchemasKeyword $
  registerKeyword unevaluatedPropertiesKeyword $
  registerKeyword unevaluatedItemsKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft-07
--
-- Draft-07 adds:
-- - if/then/else conditional keywords
-- - contentMediaType/contentEncoding
--
-- Does NOT include:
-- - dependentSchemas/dependentRequired (added in 2019-09)
-- - unevaluatedProperties/unevaluatedItems (added in 2019-09)
-- - minContains/maxContains (added in 2019-09)
-- - prefixItems (added in 2020-12)
draft07Registry :: KeywordRegistry
draft07Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  registerKeyword contentMediaTypeKeyword $
  registerKeyword contentEncodingKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  -- Conditional keywords (if/then/else, Draft-07+)
  registerKeyword ifKeyword $
  registerKeyword thenKeyword $
  registerKeyword elseKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Registry for Draft-06
--
-- Draft-06 adds:
-- - const keyword
-- - contains keyword
-- - propertyNames keyword
--
-- Does NOT include:
-- - if/then/else (added in Draft-07)
-- - contentMediaType/contentEncoding (added in Draft-07)
-- - dependentSchemas/dependentRequired (added in 2019-09)
-- - unevaluatedProperties/unevaluatedItems (added in 2019-09)
-- - minContains/maxContains (added in 2019-09)
-- - prefixItems (added in 2020-12)
draft06Registry :: KeywordRegistry
draft06Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  registerKeyword containsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  registerKeyword propertyNamesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry

-- | Backwards-compatible alias for the latest registry
standardKeywordRegistry :: KeywordRegistry
standardKeywordRegistry = draft202012Registry

-- | Registry for Draft-04 schemas
--
-- Uses Draft-04 specific numeric keywords where exclusiveMinimum/Maximum
-- are boolean modifiers rather than standalone numeric keywords.
draft04Registry :: KeywordRegistry
draft04Registry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  registerKeyword formatKeyword $
  -- Numeric validation (Draft-04 specific)
  registerKeyword D04.minimumKeyword $
  registerKeyword D04.maximumKeyword $
  registerKeyword D04.exclusiveMinimumKeyword $
  registerKeyword D04.exclusiveMaximumKeyword $
  registerKeyword multipleOfKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  registerKeyword itemsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  registerKeyword propertiesKeyword $
  registerKeyword patternPropertiesKeyword $
  registerKeyword additionalPropertiesKeyword $
  registerKeyword dependenciesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry
