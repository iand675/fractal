{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'not' composition keyword
--
-- The not keyword requires that the instance does NOT validate against the
-- provided schema. This is an applicator keyword that recursively validates
-- a subschema and inverts the result.
module Fractal.JsonSchema.Keywords.Not
  ( validateNot
  ) where

import Data.Aeson (Value)

import Fractal.JsonSchema.Types (Schema, ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure)

-- | Validate that a value does NOT satisfy the schema in 'not'
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schema: The schema that must NOT validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with no annotations if the schema does NOT validate
-- - ValidationFailure if the schema DOES validate
validateNot
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> Schema                                  -- ^ Schema that must NOT validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateNot validateSchema schema value =
  case validateSchema schema value of
    ValidationSuccess _ -> validationFailure "not" "Value matches schema in 'not'"
    ValidationFailure _ -> ValidationSuccess mempty

