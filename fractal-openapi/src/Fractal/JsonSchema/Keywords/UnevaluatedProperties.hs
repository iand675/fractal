{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'unevaluatedProperties' keyword (Draft 2019-09+)
--
-- The unevaluatedProperties keyword applies to properties that were not evaluated
-- by other keywords (properties, patternProperties, additionalProperties, or applicator keywords).
-- This is a complex keyword that requires annotation tracking across the validation process.
--
-- NOTE: Full validation for this keyword requires the architectural changes in fractal-b8g.
-- For now, this provides the keyword definition and navigation support.
module Fractal.JsonSchema.Keywords.UnevaluatedProperties
  ( unevaluatedPropertiesKeyword
  , compileUnevaluatedProperties
  , UnevaluatedPropertiesData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationUnevaluatedProperties, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  , LegacyValidateFunc, legacyValidate
  )
import Fractal.JsonSchema.Parser (parseSchema)

-- | Compiled data for unevaluatedProperties keyword
newtype UnevaluatedPropertiesData = UnevaluatedPropertiesData Schema
  deriving (Typeable)

-- | Compile the unevaluatedProperties keyword
compileUnevaluatedProperties :: CompileFunc UnevaluatedPropertiesData
compileUnevaluatedProperties value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in unevaluatedProperties: " <> T.pack (show err)
  Right schema -> Right $ UnevaluatedPropertiesData schema

-- | Validate unevaluatedProperties using the pluggable keyword system
--
-- NOTE: This is a placeholder implementation. Full validation requires annotation
-- tracking from all other keywords, which will be implemented in fractal-b8g when
-- the hardcoded validation dispatch is replaced with keyword registry dispatch.
--
-- For now, validation continues to use the hardcoded functions in Validator.hs.
validateUnevaluatedPropertiesKeyword :: LegacyValidateFunc UnevaluatedPropertiesData
validateUnevaluatedPropertiesKeyword _recursiveValidator (UnevaluatedPropertiesData _schema) _ctx _val =
  -- TODO (fractal-b8g): Implement full validation with annotation tracking
  -- For now, this is handled by the hardcoded validateUnevaluatedProperties function
  pure []

-- | Keyword definition for unevaluatedProperties
unevaluatedPropertiesKeyword :: KeywordDefinition
unevaluatedPropertiesKeyword = KeywordDefinition
  { keywordName = "unevaluatedProperties"
  , keywordScope = AnyScope
  , keywordCompile = compileUnevaluatedProperties
  , keywordValidate = legacyValidate "unevaluatedProperties" validateUnevaluatedPropertiesKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationUnevaluatedProperties (schemaValidation obj)
      _ -> Nothing
  }

