{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'pattern' keyword
--
-- The pattern keyword requires that a string value matches the specified
-- ECMA-262 regular expression pattern.
module Fractal.JsonSchema.Keywords.Pattern
  ( patternKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)
import qualified Fractal.JsonSchema.Regex as Regex

-- | Compiled data for the 'pattern' keyword
data PatternData = PatternData
  { patternRegex :: Regex.Regex
  , patternSource :: Text
  } deriving (Typeable)

instance Show PatternData where
  show (PatternData _ src) = "PatternData{pattern=" ++ T.unpack src ++ "}"

instance Eq PatternData where
  (PatternData _ src1) == (PatternData _ src2) = src1 == src2

-- | Compile function for 'pattern' keyword
compilePattern :: CompileFunc PatternData
compilePattern value _schema _ctx = case value of
  String patternStr -> case Regex.compileRegex patternStr of
    Right regex -> Right $ PatternData regex patternStr
    Left err -> Left $ "Invalid regex pattern: " <> err
  _ -> Left "pattern must be a string"

-- | Validate function for 'pattern' keyword
validatePattern :: ValidateFunc PatternData
validatePattern _recursiveValidator (PatternData regex patternStr) _ctx (String txt) =
  if Regex.matchRegex regex txt
    then []
    else ["String does not match pattern: " <> patternStr]
validatePattern _ _ _ _ = []  -- Only applies to strings

-- | The 'pattern' keyword definition
patternKeyword :: KeywordDefinition
patternKeyword = KeywordDefinition
  { keywordName = "pattern"
  , keywordScope = AnyScope
  , keywordCompile = compilePattern
  , keywordValidate = validatePattern
  , keywordNavigation = NoNavigation
  }

