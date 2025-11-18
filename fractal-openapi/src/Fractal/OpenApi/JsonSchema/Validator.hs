-- | JSON Schema validation
module Fractal.OpenApi.JsonSchema.Validator
  ( -- * Validation
    validateValue
  , ValidationResult(..)
  , ValidationError(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Fractal.OpenApi.JsonSchema.Types

-- | Validation result
data ValidationResult
  = ValidationSuccess
  | ValidationFailure [ValidationError]
  deriving (Eq, Show)

-- | Validation error
data ValidationError = ValidationError
  { errorMessage :: Text
  , errorPath :: JSONPointer
  } deriving (Eq, Show)

-- | Validate a value against a schema
--
-- TODO: Implement validation
validateValue :: Schema -> Value -> ValidationResult
validateValue = error "validateValue: not yet implemented"
