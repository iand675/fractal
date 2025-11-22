{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'oneOf' composition keyword
--
-- The oneOf keyword requires that the instance validates against EXACTLY ONE
-- schema in the provided array. This is an applicator keyword that recursively
-- validates subschemas and collects annotations from the single passing branch.
module Fractal.JsonSchema.Keywords.OneOf
  ( validateOneOf
  ) where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fractal.JsonSchema.Types (Schema, ValidationResult(..), ValidationAnnotations, validationFailure)

-- | Validate that a value satisfies EXACTLY ONE schema in oneOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas, exactly one must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with annotations from the single passing branch
-- - ValidationFailure if zero or more than one branch passes
validateOneOf
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas, exactly one must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateOneOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      successes = [anns | ValidationSuccess anns <- results]
  in case length successes of
    -- Collect annotations from the single passing branch
    1 -> ValidationSuccess $ head successes
    0 -> validationFailure "oneOf" "Value does not match any schema in oneOf"
    _ -> validationFailure "oneOf" "Value matches more than one schema in oneOf"

