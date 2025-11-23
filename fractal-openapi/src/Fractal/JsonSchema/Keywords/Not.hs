{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'not' composition keyword
--
-- The not keyword requires that the instance does NOT validate against the
-- provided schema. This is an applicator keyword that recursively validates
-- a subschema and inverts the result.
module Fractal.JsonSchema.Keywords.Not
  ( validateNot
  , notKeyword
  , NotData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Text as T

import Fractal.JsonSchema.Types (Schema, SchemaCore(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure, schemaCore, schemaNot)
import Fractal.JsonSchema.Parser (parseSchema, ParseError)
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordScope(..), CompileFunc, ValidateFunc, KeywordNavigation(..), CompilationContext(..), ValidationContext')
import Fractal.JsonSchema.Keyword (mkKeywordDefinition)

-- | Compiled data for 'not' keyword
newtype NotData = NotData Schema

-- | Compile function for 'not' keyword
compileNot :: CompileFunc NotData
compileNot value _schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in not: " <> T.pack (show err)
  Right schema -> Right $ NotData schema

-- | Validate that a value does NOT satisfy the schema in 'not'
--
-- Parameters:
-- - recursiveValidator: Recursive validation function for subschemas
-- - NotData schema: The compiled schema that must NOT validate
-- - ctx: Validation context (unused for 'not')
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with no annotations if the schema does NOT validate
-- - ValidationFailure if the schema DOES validate
validateNot
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> Schema                                  -- ^ Schema that must NOT validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateNot validateSchema schema value =
  case validateSchema schema value of
    ValidationSuccess _ -> validationFailure "not" "Value matches schema in 'not'"
    ValidationFailure _ -> ValidationSuccess mempty

-- | ValidateFunc for 'not' keyword (pluggable system)
validateNotKeyword :: ValidateFunc NotData
validateNotKeyword recursiveValidator (NotData schema) _ctx value =
  case recursiveValidator schema value of
    ValidationSuccess _ -> pure ["Value matches schema in 'not'"]
    ValidationFailure _ -> pure []

-- | Keyword definition for 'not'
notKeyword :: KeywordDefinition
notKeyword = KeywordDefinition
  { keywordName = "not"
  , keywordScope = AnyScope
  , keywordCompile = compileNot
  , keywordValidate = validateNotKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> schemaNot obj
      _ -> Nothing
  }

