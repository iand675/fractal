{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'multipleOf' keyword
--
-- The multipleOf keyword requires that a numeric value is a multiple
-- of the specified divisor.
module Fractal.JsonSchema.Keywords.MultipleOf
  ( multipleOfKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Data.Scientific as Sci

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'multipleOf' keyword
newtype MultipleOfData = MultipleOfData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'multipleOf' keyword
compileMultipleOf :: CompileFunc MultipleOfData
compileMultipleOf value _schema _ctx = case value of
  Number n | n > 0 -> Right $ MultipleOfData n
  _ -> Left "multipleOf must be a number greater than 0"

-- | Validate function for 'multipleOf' keyword
validateMultipleOf :: ValidateFunc MultipleOfData
validateMultipleOf (MultipleOfData divisor) (Number n) =
  let numDouble = Sci.toRealFloat n :: Double
      divisorDouble = Sci.toRealFloat divisor :: Double
      quotient = numDouble / divisorDouble
      remainder = quotient - fromIntegral (round quotient :: Integer)
      epsilon = 1e-10
  in if abs remainder < epsilon || abs (1 - remainder) < epsilon
     then []
     else ["Value is not a multiple of " <> T.pack (show divisor)]
validateMultipleOf _ _ = []  -- Only applies to numbers

-- | The 'multipleOf' keyword definition
multipleOfKeyword :: KeywordDefinition
multipleOfKeyword = KeywordDefinition
  { keywordName = "multipleOf"
  , keywordScope = AnyScope
  , keywordCompile = compileMultipleOf
  , keywordValidate = validateMultipleOf
  }

