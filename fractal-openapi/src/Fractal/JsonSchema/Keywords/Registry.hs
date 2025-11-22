{-# LANGUAGE OverloadedStrings #-}
-- | Keyword registries for different JSON Schema draft versions
--
-- This module provides pre-configured keyword registries for each
-- JSON Schema draft version, with draft-specific keyword implementations.
module Fractal.JsonSchema.Keywords.Registry
  ( -- * Version-Specific Registries
    draft04Registry
  , draft06Registry
  , draft07Registry
  , draft201909Registry
  , draft202012Registry
    -- * Default Registry
  , defaultRegistry
  ) where

import Fractal.JsonSchema.Keyword (KeywordRegistry, emptyKeywordRegistry, registerKeyword)
import qualified Fractal.JsonSchema.Keywords.Standard as Std

-- | Keyword registry for JSON Schema Draft-04
--
-- Draft-04 specific behaviors:
-- - exclusiveMinimum/exclusiveMaximum are booleans that modify minimum/maximum
-- - items can be a schema or array of schemas (tuple validation)
-- - No prefixItems keyword (uses items for tuple validation)
draft04Registry :: KeywordRegistry
draft04Registry =
  -- Note: Draft-04 exclusive keywords need special handling
  -- For now, using the standard (Draft-06+ style) implementations
  -- TODO: Implement Draft-04 specific minimum/maximum that check for boolean exclusives
  -- Basic validation
  registerKeyword Std.constKeyword $
  registerKeyword Std.enumKeyword $
  registerKeyword Std.typeKeyword $
  -- String validation
  registerKeyword Std.minLengthKeyword $
  registerKeyword Std.maxLengthKeyword $
  registerKeyword Std.patternKeyword $
  -- Numeric validation (Draft-04 note: exclusive keywords are booleans, not standalone)
  registerKeyword Std.minimumKeyword $
  registerKeyword Std.maximumKeyword $
  registerKeyword Std.multipleOfKeyword $
  -- Array validation
  registerKeyword Std.minItemsKeyword $
  registerKeyword Std.maxItemsKeyword $
  registerKeyword Std.uniqueItemsKeyword $
  -- Object validation
  registerKeyword Std.requiredKeyword $
  registerKeyword Std.minPropertiesKeyword $
  registerKeyword Std.maxPropertiesKeyword
  emptyKeywordRegistry

-- | Keyword registry for JSON Schema Draft-06
--
-- Draft-06 changes:
-- - exclusiveMinimum/exclusiveMaximum become standalone numeric values
-- - Contains keyword added
draft06Registry :: KeywordRegistry
draft06Registry =
  -- Basic validation
  registerKeyword Std.constKeyword $
  registerKeyword Std.enumKeyword $
  registerKeyword Std.typeKeyword $
  -- String validation
  registerKeyword Std.minLengthKeyword $
  registerKeyword Std.maxLengthKeyword $
  registerKeyword Std.patternKeyword $
  -- Numeric validation (Draft-06+: exclusive keywords are standalone numerics)
  registerKeyword Std.minimumKeyword $
  registerKeyword Std.maximumKeyword $
  registerKeyword Std.multipleOfKeyword $
  registerKeyword Std.exclusiveMinimumKeyword $
  registerKeyword Std.exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword Std.minItemsKeyword $
  registerKeyword Std.maxItemsKeyword $
  registerKeyword Std.uniqueItemsKeyword $
  -- Object validation
  registerKeyword Std.requiredKeyword $
  registerKeyword Std.minPropertiesKeyword $
  registerKeyword Std.maxPropertiesKeyword
  emptyKeywordRegistry

-- | Keyword registry for JSON Schema Draft-07
--
-- Draft-07 changes:
-- - if/then/else keywords added
-- - readOnly/writeOnly keywords added
draft07Registry :: KeywordRegistry
draft07Registry = draft06Registry  -- Same as Draft-06 for now

-- | Keyword registry for JSON Schema Draft 2019-09
--
-- 2019-09 changes:
-- - unevaluatedProperties/unevaluatedItems keywords added
-- - dependentSchemas/dependentRequired replace dependencies
-- - $recursiveRef/$recursiveAnchor for recursive schemas
draft201909Registry :: KeywordRegistry
draft201909Registry = draft07Registry  -- Same as Draft-07 for now

-- | Keyword registry for JSON Schema Draft 2020-12
--
-- 2020-12 changes:
-- - prefixItems replaces tuple-style items
-- - $dynamicRef/$dynamicAnchor replace $recursiveRef/$recursiveAnchor
draft202012Registry :: KeywordRegistry
draft202012Registry = draft201909Registry  -- Same as 2019-09 for now

-- | Default registry (uses latest draft - 2020-12)
defaultRegistry :: KeywordRegistry
defaultRegistry = draft202012Registry
