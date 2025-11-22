-- | Keyword validation phase
--
-- This module implements the validate phase of the two-phase keyword system.
-- During validation, compiled keyword data is used to validate instance values.
module Fractal.JsonSchema.Keyword.Validate
  ( -- * Validation
    validateKeywords
    -- * Validation Context
  , ValidationContext(..)
  , buildValidationContext
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Compile (CompiledKeywords(..), lookupCompiledKeyword)

-- | Context for validation phase
--
-- Provides information needed during validation, such as the current
-- path in the instance being validated.
data ValidationContext = ValidationContext
  { validationInstancePath :: [Text]
    -- ^ Path to current location in instance (for error reporting)
  , validationSchemaPath :: [Text]
    -- ^ Path to current location in schema (for error reporting)
  }
  deriving (Show)

-- | Build a validation context
buildValidationContext :: [Text] -> [Text] -> ValidationContext
buildValidationContext instancePath schemaPath =
  ValidationContext
    { validationInstancePath = instancePath
    , validationSchemaPath = schemaPath
    }

-- | Validate all keywords
--
-- Validates an instance value against all compiled keywords.
-- Returns a list of all validation errors from all keywords.
validateKeywords
  :: CompiledKeywords            -- ^ Compiled keywords to validate against
  -> Value                       -- ^ Instance value to validate
  -> ValidationContext           -- ^ Validation context
  -> [Text]
validateKeywords (CompiledKeywords compiled) value _ctx =
  -- Validate each compiled keyword using its type-erased validate function
  concatMap validateOne (Map.elems compiled)
  where
    validateOne :: CompiledKeyword -> [Text]
    validateOne (CompiledKeyword _name _data validateFn _adjacent) =
      -- Call the type-erased validate function (closure over compiled data)
      validateFn value
