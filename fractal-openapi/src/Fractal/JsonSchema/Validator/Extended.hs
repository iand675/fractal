-- | Extended validator with custom keyword support
--
-- This module extends the standard validator to validate instances
-- against ExtendedSchema (schemas with compiled custom keywords).
module Fractal.JsonSchema.Validator.Extended
  ( -- * Extended Validation
    validateExtended
  , validateExtendedWithConfig
    -- * Validation with Custom Keywords
  , ExtendedValidationResult(..)
  , toExtendedValidationResult
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Fractal.JsonSchema.Types (ValidationResult, ValidationConfig, isSuccess)
import Fractal.JsonSchema.Validator (validateValue, defaultValidationConfig)
import Fractal.JsonSchema.Parser.Extended (ExtendedSchema(..), fromExtendedSchema)
import Fractal.JsonSchema.Keyword.Validate (validateKeywords, ValidationContext, buildValidationContext)
import Fractal.JsonSchema.Validator.Result (ValidationError(..))

-- | Extended validation result
--
-- Combines results from standard validation and custom keyword validation.
data ExtendedValidationResult = ExtendedValidationResult
  { extendedStandardResult :: ValidationResult
    -- ^ Result from standard keyword validation
  , extendedCustomErrors :: [Text]
    -- ^ Errors from custom keyword validation
  , extendedIsValid :: Bool
    -- ^ Overall validation status (standard AND custom)
  }
  deriving (Show)

-- | Convert a standard ValidationResult to ExtendedValidationResult
toExtendedValidationResult :: ValidationResult -> ExtendedValidationResult
toExtendedValidationResult standardResult = ExtendedValidationResult
  { extendedStandardResult = standardResult
  , extendedCustomErrors = []
  , extendedIsValid = isSuccess standardResult
  }

-- | Validate an instance against an ExtendedSchema
--
-- This combines:
-- 1. Standard validation against the base schema
-- 2. Custom keyword validation using compiled keywords
validateExtended
  :: ExtendedSchema          -- ^ Schema with compiled custom keywords
  -> Value                   -- ^ Instance to validate
  -> ExtendedValidationResult
validateExtended = validateExtendedWithConfig defaultValidationConfig

-- | Validate with custom config
validateExtendedWithConfig
  :: ValidationConfig        -- ^ Validation configuration
  -> ExtendedSchema          -- ^ Schema with compiled custom keywords
  -> Value                   -- ^ Instance to validate
  -> ExtendedValidationResult
validateExtendedWithConfig config extSchema value =
  let -- Validate with standard validator
      standardResult = validateValue config (fromExtendedSchema extSchema) value

      -- Validate with custom keywords
      ctx = buildValidationContext [] []
      customErrors = validateKeywords (extendedCompiledKeywords extSchema) value ctx

      -- Combine results
      overallValid = isSuccess standardResult && null customErrors

  in ExtendedValidationResult
    { extendedStandardResult = standardResult
    , extendedCustomErrors = customErrors
    , extendedIsValid = overallValid
    }
