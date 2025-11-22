{-# LANGUAGE OverloadedStrings #-}
-- | Type keyword validation
--
-- Validates the 'type' keyword which constrains a value to a specific JSON type
-- or a union of types.
module Fractal.JsonSchema.Keywords.Type
  ( validateTypeConstraint
  ) where

import Fractal.JsonSchema.Types
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sci

-- | Validate type constraint
--
-- The type keyword restricts a value to a specific JSON type or union of types.
-- For arrays of types, validation succeeds if the value matches any type in the array.
validateTypeConstraint :: SchemaObject -> Value -> ValidationResult
validateTypeConstraint obj val = case schemaType obj of
  Nothing -> ValidationSuccess mempty
  Just (One expectedType) -> validateType expectedType val
  Just (Many types) -> validateTypeUnion (NE.toList types) val
  where
    validateType :: SchemaType -> Value -> ValidationResult
    validateType NullType Null = ValidationSuccess mempty
    validateType NullType _ = validationFailure "type" "Expected null"
    validateType BooleanType (Bool _) = ValidationSuccess mempty
    validateType BooleanType _ = validationFailure "type" "Expected boolean"
    validateType StringType (String _) = ValidationSuccess mempty
    validateType StringType _ = validationFailure "type" "Expected string"
    validateType NumberType (Number _) = ValidationSuccess mempty
    validateType NumberType _ = validationFailure "type" "Expected number"
    validateType IntegerType (Number n) =
      if Sci.isInteger n
        then ValidationSuccess mempty
        else validationFailure "type" "Expected integer"
    validateType IntegerType _ = validationFailure "type" "Expected integer"
    validateType ObjectType (Object _) = ValidationSuccess mempty
    validateType ObjectType _ = validationFailure "type" "Expected object"
    validateType ArrayType (Array _) = ValidationSuccess mempty
    validateType ArrayType _ = validationFailure "type" "Expected array"

    validateTypeUnion :: [SchemaType] -> Value -> ValidationResult
    validateTypeUnion types v =
      if any (\t -> isSuccess $ validateType t v) types
        then ValidationSuccess mempty
        else validationFailure "type" $ "Expected one of: " <> T.intercalate ", " (map (T.pack . show) types)
