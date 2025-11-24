{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
-- | Implementation of the 'maxProperties' keyword
--
-- The maxProperties keyword requires that an object value has at most
-- the specified number of properties.
module Fractal.JsonSchema.Keywords.MaxProperties
  ( maxPropertiesKeyword
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci
import qualified Data.Aeson.KeyMap as KeyMap

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..), LegacyValidateFunc, legacyValidate)
import Fractal.JsonSchema.Types (Schema)

-- | Compiled data for the 'maxProperties' keyword
newtype MaxPropertiesData = MaxPropertiesData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maxProperties' keyword
compileMaxProperties :: CompileFunc MaxPropertiesData
compileMaxProperties value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MaxPropertiesData (fromInteger $ truncate n)
  _ -> Left "maxProperties must be a non-negative integer"

-- | Validate function for 'maxProperties' keyword
validateMaxProperties :: ValidateFunc MaxPropertiesData
validateMaxProperties = legacyValidate "maxProperties" validateMaxPropertiesLegacy

validateMaxPropertiesLegacy :: LegacyValidateFunc MaxPropertiesData
validateMaxPropertiesLegacy _recursiveValidator (MaxPropertiesData maxProps) _ctx (Object objMap) =
  let propCount = fromIntegral (KeyMap.size objMap) :: Natural
  in if propCount <= maxProps
     then pure []
     else pure ["Object has " <> T.pack (show propCount) <> " properties, but maxProperties is " <> T.pack (show maxProps)]
validateMaxPropertiesLegacy _ _ _ _ = pure []  -- Only applies to objects

-- | The 'maxProperties' keyword definition
maxPropertiesKeyword :: KeywordDefinition
maxPropertiesKeyword = KeywordDefinition
  { keywordName = "maxProperties"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxProperties
  , keywordValidate = validateMaxProperties
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

