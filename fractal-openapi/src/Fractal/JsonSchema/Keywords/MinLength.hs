{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'minLength' keyword
--
-- The minLength keyword requires that a string value has at least the
-- specified number of Unicode characters.
module Fractal.JsonSchema.Keywords.MinLength
  ( minLengthKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'minLength' keyword
newtype MinLengthData = MinLengthData Int
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minLength' keyword
compileMinLength :: CompileFunc MinLengthData
compileMinLength value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 -> Right $ MinLengthData (truncate n)
  _ -> Left "minLength must be a non-negative integer"

-- | Validate function for 'minLength' keyword
validateMinLength :: ValidateFunc MinLengthData
validateMinLength (MinLengthData minLen) (String txt) =
  if T.length txt >= minLen
    then []
    else ["String length " <> T.pack (show (T.length txt)) <> " is less than minLength " <> T.pack (show minLen)]
validateMinLength _ _ = []  -- Only applies to strings

-- | The 'minLength' keyword definition
minLengthKeyword :: KeywordDefinition
minLengthKeyword = KeywordDefinition
  { keywordName = "minLength"
  , keywordScope = AnyScope
  , keywordCompile = compileMinLength
  , keywordValidate = validateMinLength
  , keywordNavigation = NoNavigation
  }

