{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- | Implementation of the 'maxLength' keyword
--
-- The maxLength keyword requires that a string value has at most the
-- specified number of Unicode characters.
module Fractal.JsonSchema.Keywords.MaxLength
  ( maxLengthKeyword
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'maxLength' keyword
newtype MaxLengthData = MaxLengthData Int
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maxLength' keyword
compileMaxLength :: CompileFunc MaxLengthData
compileMaxLength value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 -> Right $ MaxLengthData (truncate n)
  _ -> Left "maxLength must be a non-negative integer"

-- | Validate function for 'maxLength' keyword
validateMaxLength :: ValidateFunc MaxLengthData
validateMaxLength _recursiveValidator (MaxLengthData maxLen) _ctx (String txt) =
  if T.length txt <= maxLen
    then pure []
    else pure ["String length " <> T.pack (show (T.length txt)) <> " exceeds maxLength " <> T.pack (show maxLen)]
validateMaxLength _ _ _ _ = pure []  -- Only applies to strings

-- | The 'maxLength' keyword definition
maxLengthKeyword :: KeywordDefinition
maxLengthKeyword = KeywordDefinition
  { keywordName = "maxLength"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxLength
  , keywordValidate = validateMaxLength
  , keywordNavigation = NoNavigation
  }

