-- | Keyword validation phase
--
-- This module implements the validate phase of the two-phase keyword system.
-- During validation, compiled keyword data is used to validate instance values.
module Fractal.JsonSchema.Keyword.Validate
  ( -- * Validation
    validateKeywords
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Typeable (Typeable, cast)

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Compile (CompiledKeywords(..), lookupCompiledKeyword)
import Fractal.JsonSchema.Types (Schema, ValidationResult)

-- | Validate all keywords
--
-- Validates an instance value against all compiled keywords.
-- Returns a list of all validation errors from all keywords.
--
-- The recursive validator parameter allows keywords to recursively validate
-- subschemas (needed for applicator keywords like allOf, items, etc).
validateKeywords
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> CompiledKeywords                      -- ^ Compiled keywords to validate against
  -> Value                                 -- ^ Instance value to validate
  -> ValidationContext'                    -- ^ Validation context (paths)
  -> [Text]
validateKeywords recursiveValidator (CompiledKeywords compiled) value ctx =
  -- Validate each compiled keyword using its type-erased validate function
  concatMap validateOne (Map.elems compiled)
  where
    validateOne :: CompiledKeyword -> [Text]
    validateOne (CompiledKeyword _name _data validateFn _adjacent) =
      -- Call the type-erased validate function with recursive validator and context
      validateFn recursiveValidator ctx value
