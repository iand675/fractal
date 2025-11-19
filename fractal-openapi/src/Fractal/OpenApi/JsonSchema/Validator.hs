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
--
-- Per JSON Schema spec, patterns use Unicode semantics for Unicode-aware features.
-- We enable Unicode mode for patterns that need it based on their content.
validatePattern :: Text -> Text -> Either Text Bool
validatePattern pattern value =
  unsafePerformIO $ do
    let flags = if needsUnicodeMode pattern then [Regex.Unicode] else []
    compiled <- Regex.compileText pattern flags
    case compiled of
      Left err -> return $ Left $ T.pack $ "Invalid regex pattern: " <> err
      Right regex -> do
        result <- Regex.test regex (TE.encodeUtf8 value)
        return $ Right result
  where
    -- Check if pattern needs Unicode mode
    -- Enable only for explicit Unicode features that require the Unicode flag
    needsUnicodeMode pat =
      T.isInfixOf "\\p{" pat ||  -- Unicode property escapes
      T.isInfixOf "\\P{" pat ||  -- Negated Unicode properties
      T.isInfixOf "\\u{" pat     -- Unicode codepoint escapes (e.g., \u{1F600})

-- | Validate a value against a schema
--
-- TODO: Implement full validation with pattern support
validateValue :: Schema -> Value -> ValidationResult
validateValue = error "validateValue: not yet implemented"
