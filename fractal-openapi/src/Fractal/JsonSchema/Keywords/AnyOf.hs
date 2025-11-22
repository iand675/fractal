{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'anyOf' composition keyword
--
-- The anyOf keyword requires that the instance validates against AT LEAST ONE
-- schema in the provided array. This is an applicator keyword that recursively
-- validates subschemas and collects annotations from all passing branches.
module Fractal.JsonSchema.Keywords.AnyOf
  ( validateAnyOf
  ) where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import Fractal.JsonSchema.Types (Schema, ValidationResult(..), ValidationAnnotations, validationFailure)

-- | Validate that a value satisfies AT LEAST ONE schema in anyOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas, at least one must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from ALL passing branches
-- - ValidationFailure if NO branches pass
validateAnyOf
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas, at least one must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAnyOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      successes = [anns | ValidationSuccess anns <- results]
  in if null successes
    then validationFailure "anyOf" "Value does not match any schema in anyOf"
    -- Collect annotations from ALL passing branches in anyOf
    else ValidationSuccess $ mconcat successes

