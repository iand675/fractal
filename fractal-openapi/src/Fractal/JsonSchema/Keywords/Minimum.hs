{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'minimum' keyword
--
-- The minimum keyword requires that a numeric value is greater than or
-- equal to the specified minimum value.
module Fractal.JsonSchema.Keywords.Minimum
  ( minimumKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'minimum' keyword
newtype MinimumData = MinimumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minimum' keyword
compileMinimum :: CompileFunc MinimumData
compileMinimum value _schema _ctx = case value of
  Number n -> Right $ MinimumData n
  _ -> Left "minimum must be a number"

-- | Validate function for 'minimum' keyword
validateMinimum :: ValidateFunc MinimumData
validateMinimum (MinimumData minVal) (Number n) =
  if n >= minVal
    then []
    else ["Value " <> T.pack (show n) <> " is less than minimum " <> T.pack (show minVal)]
validateMinimum _ _ = []  -- Only applies to numbers

-- | The 'minimum' keyword definition
minimumKeyword :: KeywordDefinition
minimumKeyword = KeywordDefinition
  { keywordName = "minimum"
  , keywordScope = AnyScope
  , keywordCompile = compileMinimum
  , keywordValidate = validateMinimum
  , keywordNavigation = NoNavigation
  }

