{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of conditional keywords (if/then/else)
--
-- The if/then/else keywords provide conditional schema application (Draft-07+).
-- If the 'if' schema validates, apply 'then' (if present).
-- If the 'if' schema fails, apply 'else' (if present).
--
-- This is an applicator keyword that recursively validates subschemas and
-- properly collects annotations from the applied branch.
module Fractal.JsonSchema.Keywords.Conditional
  ( validateConditional
  ) where

import Data.Aeson (Value)

import Fractal.JsonSchema.Types (Schema, ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations)

-- | Validate conditional keywords (if/then/else)
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - ifSchema: The condition schema to test
-- - thenSchema: Optional schema to apply if condition validates
-- - elseSchema: Optional schema to apply if condition fails
-- - value: The instance value to validate
--
-- Returns:
-- - If 'if' validates and 'then' is present: result of 'then' with 'if' annotations
-- - If 'if' validates and 'then' is absent: 'if' annotations only
-- - If 'if' fails and 'else' is present: result of 'else'
-- - If 'if' fails and 'else' is absent: success with no annotations
validateConditional
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> Schema                                  -- ^ The 'if' condition schema
  -> Maybe Schema                            -- ^ Optional 'then' schema
  -> Maybe Schema                            -- ^ Optional 'else' schema
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateConditional validateSchema ifSchema mThenSchema mElseSchema value =
  case validateSchema ifSchema value of
    ValidationSuccess ifAnns ->
      -- If validates, apply then (if present) and combine annotations
      case mThenSchema of
        Just thenSchema ->
          case validateSchema thenSchema value of
            ValidationSuccess thenAnns -> ValidationSuccess (ifAnns <> thenAnns)
            ValidationFailure errs -> ValidationFailure errs
        Nothing -> ValidationSuccess ifAnns  -- No then, keep if annotations
    ValidationFailure _ ->
      -- If fails, apply else (if present)
      case mElseSchema of
        Just elseSchema -> validateSchema elseSchema value
        Nothing -> ValidationSuccess mempty  -- No else, succeed with no annotations

