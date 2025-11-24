{-# LANGUAGE PatternSynonyms #-}

module Fractal.JsonSchema.TestHelpers
  ( runValidation
  , validationErrors
  , runValidationErrors
  ) where

import Control.Monad.Trans.Reader (Reader, runReader)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)

import Fractal.JsonSchema.Validator (defaultValidationConfig)
import Fractal.JsonSchema.Types
  ( ValidationResult
  , ValidationConfig
  , ValidationErrors(..)
  , pattern ValidationFailure
  )
import Fractal.JsonSchema.Validator.Result (ValidationError(..))

-- | Run a Reader-based validation action with the default config.
runValidation :: Reader ValidationConfig ValidationResult -> ValidationResult
runValidation action = runReader action defaultValidationConfig

-- | Extract error messages from a validation result.
validationErrors :: ValidationResult -> [Text]
validationErrors (ValidationFailure (ValidationErrors errs)) =
  map errorMessage (NE.toList errs)
validationErrors _ = []

-- | Run an action and collect error messages.
runValidationErrors :: Reader ValidationConfig ValidationResult -> [Text]
runValidationErrors = validationErrors . runValidation

