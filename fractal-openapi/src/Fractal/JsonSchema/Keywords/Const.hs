{-# LANGUAGE OverloadedStrings #-}
-- | Const keyword validation
--
-- Validates the 'const' keyword (JSON Schema draft-06+) which constrains
-- a value to be exactly equal to a specified constant value.
module Fractal.JsonSchema.Keywords.Const
  ( validateConstConstraint
  ) where

import Fractal.JsonSchema.Types
import Data.Aeson (Value)

-- | Validate const constraint (draft-06+)
--
-- The const keyword validates that a value is exactly equal to the
-- specified constant using JSON equality.
validateConstConstraint :: SchemaObject -> Value -> ValidationResult
validateConstConstraint obj val = case schemaConst obj of
  Nothing -> ValidationSuccess mempty
  Just expected ->
    if val == expected
      then ValidationSuccess mempty
      else validationFailure "const" "Value does not match const"
