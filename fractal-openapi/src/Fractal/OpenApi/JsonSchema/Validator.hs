-- | JSON Schema validation
module Fractal.OpenApi.JsonSchema.Validator
  ( -- * Validation
    validateValue
  , validatePattern
  , ValidationResult(..)
  , ValidationError(..)
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Text.Regex.ECMA262 as Regex
import Fractal.OpenApi.JsonSchema.Types
import System.IO.Unsafe (unsafePerformIO)

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

-- | Validate a string value against a regex pattern using ECMAScript 262 regex
--
-- This uses the ecma262-regex package which provides full ECMAScript 2023
-- regex compliance, matching the JSON Schema specification requirements.
validatePattern :: Text -> Text -> Either Text Bool
validatePattern pattern value =
  unsafePerformIO $ do
    compiled <- Regex.compileText pattern []
    case compiled of
      Left err -> return $ Left $ T.pack $ "Invalid regex pattern: " <> err
      Right regex -> do
        result <- Regex.test regex (TE.encodeUtf8 value)
        return $ Right result

-- | Validate a value against a schema
--
-- TODO: Implement full validation with pattern support
validateValue :: Schema -> Value -> ValidationResult
validateValue = error "validateValue: not yet implemented"
