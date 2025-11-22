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

-- | Registry containing all standard keywords
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
  emptyKeywordRegistry
