{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'exclusiveMaximum' keyword (Draft-06+ standalone numeric)
--
-- The exclusiveMaximum keyword requires that a numeric value is strictly
-- less than the specified maximum value.
module Fractal.JsonSchema.Keywords.ExclusiveMaximum
  ( exclusiveMaximumKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'exclusiveMaximum' keyword (Draft-06+ standalone numeric)
newtype ExclusiveMaximumData = ExclusiveMaximumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'exclusiveMaximum' keyword
compileExclusiveMaximum :: CompileFunc ExclusiveMaximumData
compileExclusiveMaximum value _schema _ctx = case value of
  Number n -> Right $ ExclusiveMaximumData n
  _ -> Left "exclusiveMaximum must be a number"

-- | Validate function for 'exclusiveMaximum' keyword
validateExclusiveMaximum :: ValidateFunc ExclusiveMaximumData
validateExclusiveMaximum (ExclusiveMaximumData maxVal) (Number n) =
  if n < maxVal
    then []
    else ["Value " <> T.pack (show n) <> " must be less than exclusiveMaximum " <> T.pack (show maxVal)]
validateExclusiveMaximum _ _ = []  -- Only applies to numbers

-- | The 'exclusiveMaximum' keyword definition (Draft-06+ style)
exclusiveMaximumKeyword :: KeywordDefinition
exclusiveMaximumKeyword = KeywordDefinition
  { keywordName = "exclusiveMaximum"
  , keywordScope = AnyScope
  , keywordCompile = compileExclusiveMaximum
  , keywordValidate = validateExclusiveMaximum
  }

