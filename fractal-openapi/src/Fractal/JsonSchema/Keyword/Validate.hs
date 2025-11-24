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
import Data.Maybe (isJust, isNothing)
import qualified Data.Map.Strict as Map

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Compile (CompiledKeywords(..))
import Fractal.JsonSchema.Types
  ( Schema
  , ValidationResult
  , ValidationConfig
  , ValidationAnnotations
  , pattern ValidationSuccess
  , pattern ValidationFailure
  )

-- | Validate all keywords
--
-- Validates an instance value against all compiled keywords in two phases:
-- 1. Regular keywords (validated first)
-- 2. Post-validation keywords (validated after, with access to annotations from regular keywords)
--
-- Returns validation result with combined errors and annotations.
--
-- The recursive validator parameter allows keywords to recursively validate
-- subschemas (needed for applicator keywords like allOf, items, etc).
--
-- The ValidationConfig is passed to each keyword's validate function via the Reader monad.
--
-- Additional annotations (e.g., from $ref validation) are included in the annotations
-- passed to post-validation keywords.
validateKeywords
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator for subschemas
  -> CompiledKeywords                      -- ^ Compiled keywords to validate against
  -> Value                                 -- ^ Instance value to validate
  -> ValidationContext'                    -- ^ Validation context (paths)
  -> ValidationConfig                      -- ^ Validation configuration (passed to Reader)
  -> ValidationAnnotations                 -- ^ Additional annotations to include (e.g., from $ref)
  -> ValidationResult
validateKeywords recursiveValidator (CompiledKeywords compiled) value ctx config additionalAnns =
  -- Phase 1: Validate regular keywords
  let regularKeywords = [ck | ck <- Map.elems compiled, isNothing (compiledPostValidate ck)]
      regularResults = map (validateRegular recursiveValidator ctx value config) regularKeywords
      regularFailures = [errs | ValidationFailure errs <- regularResults]
      regularAnnotations = [anns | ValidationSuccess anns <- regularResults]
      combinedAnnotations = additionalAnns <> mconcat regularAnnotations
  in case regularFailures of
    -- If regular keywords failed, return failure immediately
    (e:es) -> ValidationFailure (foldl (<>) e es)
    -- Phase 2: Validate post-validation keywords with annotations
    [] ->
      let postKeywords = [ck | ck <- Map.elems compiled, isJust (compiledPostValidate ck)]
          postResults = map (validatePost recursiveValidator ctx value config combinedAnnotations) postKeywords
          postFailures = [errs | ValidationFailure errs <- postResults]
          postAnnotations = [anns | ValidationSuccess anns <- postResults]
          allAnnotations = combinedAnnotations <> mconcat postAnnotations
      in case postFailures of
        [] -> ValidationSuccess allAnnotations
        (e:es) -> ValidationFailure (foldl (<>) e es)
  where
    validateRegular :: (Schema -> Value -> ValidationResult) -> ValidationContext' -> Value -> ValidationConfig -> CompiledKeyword -> ValidationResult
    validateRegular recVal valCtx val cfg (CompiledKeyword _name _data validateFn _postValidate _adjacent) =
      runReader (validateFn recVal valCtx val) cfg

    validatePost :: (Schema -> Value -> ValidationResult) -> ValidationContext' -> Value -> ValidationConfig -> ValidationAnnotations -> CompiledKeyword -> ValidationResult
    validatePost recVal valCtx val cfg anns (CompiledKeyword _name _data _validateFn (Just postValidateFn) _adjacent) =
      runReader (postValidateFn recVal valCtx val anns) cfg
    validatePost _ _ _ _ _ _ = error "validatePost called on keyword without post-validation function"
