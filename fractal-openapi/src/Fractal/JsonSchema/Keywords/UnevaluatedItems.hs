{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'unevaluatedItems' keyword (Draft 2019-09+)
--
-- The unevaluatedItems keyword applies to array items that were not evaluated
-- by other keywords (items, prefixItems, contains, or applicator keywords).
-- This is a complex keyword that requires annotation tracking across the validation process.
--
-- NOTE: Full validation for this keyword requires the architectural changes in fractal-b8g.
-- For now, this provides the keyword definition and navigation support.
module Fractal.JsonSchema.Keywords.UnevaluatedItems
  ( unevaluatedItemsKeyword
  , compileUnevaluatedItems
  , UnevaluatedItemsData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationUnevaluatedItems, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  )
import Fractal.JsonSchema.Parser (parseSchema)

-- | Compiled data for unevaluatedItems keyword
newtype UnevaluatedItemsData = UnevaluatedItemsData Schema
  deriving (Typeable)

-- | Compile the unevaluatedItems keyword
compileUnevaluatedItems :: CompileFunc UnevaluatedItemsData
compileUnevaluatedItems value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in unevaluatedItems: " <> T.pack (show err)
  Right schema -> Right $ UnevaluatedItemsData schema

-- | Validate unevaluatedItems using the pluggable keyword system
--
-- NOTE: This is a placeholder implementation. Full validation requires annotation
-- tracking from all other keywords, which will be implemented in fractal-b8g when
-- the hardcoded validation dispatch is replaced with keyword registry dispatch.
--
-- For now, validation continues to use the hardcoded functions in Validator.hs.
validateUnevaluatedItemsKeyword :: ValidateFunc UnevaluatedItemsData
validateUnevaluatedItemsKeyword _recursiveValidator (UnevaluatedItemsData _schema) _ctx _val =
  -- TODO (fractal-b8g): Implement full validation with annotation tracking
  -- For now, this is handled by the hardcoded validateUnevaluatedItems function
  pure []

-- | Keyword definition for unevaluatedItems
unevaluatedItemsKeyword :: KeywordDefinition
unevaluatedItemsKeyword = KeywordDefinition
  { keywordName = "unevaluatedItems"
  , keywordScope = AnyScope
  , keywordCompile = compileUnevaluatedItems
  , keywordValidate = validateUnevaluatedItemsKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationUnevaluatedItems (schemaValidation obj)
      _ -> Nothing
  }

