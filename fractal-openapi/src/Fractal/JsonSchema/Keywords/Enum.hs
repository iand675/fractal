{-# LANGUAGE OverloadedStrings #-}
-- | Enum keyword validation
--
-- Validates the 'enum' keyword which constrains a value to one of a fixed set
-- of allowed values.
module Fractal.JsonSchema.Keywords.Enum
  ( validateEnumConstraint
  ) where

import Fractal.JsonSchema.Types
import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE

-- | Validate enum constraint
--
-- The enum keyword validates that a value is equal to one of the elements
-- in the specified array. Values are compared using JSON equality.
validateEnumConstraint :: SchemaObject -> Value -> ValidationResult
validateEnumConstraint obj val = case schemaEnum obj of
  Nothing -> ValidationSuccess mempty
  Just allowedValues ->
    if val `elem` NE.toList allowedValues
      then ValidationSuccess mempty
      else validationFailure "enum" $ "Value not in enum: " <> T.pack (show val)
