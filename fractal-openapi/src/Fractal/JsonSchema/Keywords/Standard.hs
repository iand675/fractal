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
  , standardKeywordRegistry
  , draft04Registry
  ) where

import Fractal.JsonSchema.Keyword (KeywordRegistry, emptyKeywordRegistry, registerKeyword)
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
import Fractal.JsonSchema.Keywords.DependentRequired (dependentRequiredKeyword)
import Fractal.JsonSchema.Keywords.DependentSchemas (dependentSchemasKeyword)
import Fractal.JsonSchema.Keywords.UnevaluatedProperties (unevaluatedPropertiesKeyword)
import Fractal.JsonSchema.Keywords.UnevaluatedItems (unevaluatedItemsKeyword)
import Fractal.JsonSchema.Keywords.FormatKeyword (formatKeyword)

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

-- | Registry containing all standard keywords (Draft-06+)
--
-- This registry can be extended with custom keywords or used as-is
-- for standard JSON Schema validation.
standardKeywordRegistry :: KeywordRegistry
standardKeywordRegistry =
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
  -- Navigable keywords (for $ref resolution)
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Not.notKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry
