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
    -- * Object Keywords
  , requiredKeyword
  , minPropertiesKeyword
  , maxPropertiesKeyword
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
-- Object keywords
import Fractal.JsonSchema.Keywords.Required (requiredKeyword)
import Fractal.JsonSchema.Keywords.MinProperties (minPropertiesKeyword)
import Fractal.JsonSchema.Keywords.MaxProperties (maxPropertiesKeyword)
-- Navigable keywords (for $ref resolution)
import qualified Fractal.JsonSchema.Keywords.Navigation as Nav
import qualified Fractal.JsonSchema.Keywords.AllOf as AllOf
import qualified Fractal.JsonSchema.Keywords.AnyOf as AnyOf
import qualified Fractal.JsonSchema.Keywords.OneOf as OneOf
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
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword Nav.propertiesKeyword $
  registerKeyword Nav.patternPropertiesKeyword $
  registerKeyword Nav.additionalPropertiesKeyword $
  registerKeyword Nav.itemsKeyword $
  registerKeyword Nav.prefixItemsKeyword $
  registerKeyword Nav.containsKeyword $
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Nav.notKeyword $
  registerKeyword Nav.ifKeyword $
  registerKeyword Nav.thenKeyword $
  registerKeyword Nav.elseKeyword $
  registerKeyword Nav.dependentSchemasKeyword $
  registerKeyword Nav.propertyNamesKeyword $
  registerKeyword Nav.unevaluatedPropertiesKeyword $
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
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword $
  -- Navigable keywords (for $ref resolution)
  registerKeyword Nav.propertiesKeyword $
  registerKeyword Nav.patternPropertiesKeyword $
  registerKeyword Nav.additionalPropertiesKeyword $
  registerKeyword Nav.itemsKeyword $
  registerKeyword Nav.containsKeyword $
  registerKeyword AllOf.allOfKeyword $
  registerKeyword AnyOf.anyOfKeyword $
  registerKeyword OneOf.oneOfKeyword $
  registerKeyword Nav.notKeyword $
  registerKeyword Nav.ifKeyword $
  registerKeyword Nav.thenKeyword $
  registerKeyword Nav.elseKeyword $
  registerKeyword Nav.dependentSchemasKeyword $
  registerKeyword Nav.propertyNamesKeyword $
  registerKeyword Nav.defsKeyword $
  emptyKeywordRegistry
