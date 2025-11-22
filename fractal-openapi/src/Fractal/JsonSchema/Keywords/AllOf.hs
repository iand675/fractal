{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'allOf' composition keyword
--
-- The allOf keyword requires that the instance validates against ALL schemas
-- in the provided array. This is an applicator keyword that recursively 
-- validates subschemas and collects annotations from all branches.
module Fractal.JsonSchema.Keywords.AllOf
  ( validateAllOf
  ) where

import Data.Aeson (Value)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

import Fractal.JsonSchema.Types (Schema, ValidationResult(..), ValidationErrors, ValidationAnnotations)

-- | Validate that a value satisfies ALL schemas in allOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas that all must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from all branches (all must pass)
-- - ValidationFailure with combined errors if any branch fails
validateAllOf 
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas that all must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAllOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      failures = [errs | ValidationFailure errs <- results]
      annotations = [anns | ValidationSuccess anns <- results]
  in case failures of
    -- Collect annotations from ALL branches in allOf (all must pass)
    [] -> ValidationSuccess $ mconcat annotations
    (e:es) -> ValidationFailure $ foldl (<>) e es
