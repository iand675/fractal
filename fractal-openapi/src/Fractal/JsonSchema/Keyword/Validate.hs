{-# LANGUAGE PatternSynonyms #-}

-- | Keyword validation phase
--
-- This module implements the validate phase of the two-phase keyword system.
-- During validation, compiled keyword data is used to validate instance values.
module Fractal.JsonSchema.Keyword.Validate
  ( -- * Validation
    validateKeywords
  ) where

import Control.Monad.Reader (runReader)
import Data.Aeson (Value)
import qualified Data.Map.Strict as Map

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Compile (CompiledKeywords(..))
import Fractal.JsonSchema.Types
  ( Schema
  , ValidationResult
  , ValidationConfig
  , pattern ValidationSuccess
  , pattern ValidationFailure
  )

-- | Validate all keywords
--
-- Validates an instance value against all compiled keywords.
-- Returns a list of all validation errors from all keywords.
--
-- The recursive validator parameter allows keywords to recursively validate
-- subschemas (needed for applicator keywords like allOf, items, etc).
--
-- The ValidationConfig is passed to each keyword's validate function via the Reader monad.
validateKeywords
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> CompiledKeywords                      -- ^ Compiled keywords to validate against
  -> Value                                 -- ^ Instance value to validate
  -> ValidationContext'                    -- ^ Validation context (paths)
  -> ValidationConfig                      -- ^ Validation configuration (passed to Reader)
  -> ValidationResult
validateKeywords recursiveValidator (CompiledKeywords compiled) value ctx config =
  case failures of
    [] ->
      ValidationSuccess (mconcat annotations)
    (e:es) ->
      ValidationFailure (foldl (<>) e es)
  where
    results = map validateOne (Map.elems compiled)

    failures =
      [ errs
      | result <- results
      , ValidationFailure errs <- pure result
      ]

    annotations =
      [ anns
      | result <- results
      , ValidationSuccess anns <- pure result
      ]

    validateOne :: CompiledKeyword -> ValidationResult
    validateOne (CompiledKeyword _name _data validateFn _adjacent) =
      runReader (validateFn recursiveValidator ctx value) config
