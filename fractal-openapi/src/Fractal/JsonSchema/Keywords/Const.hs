{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'const' keyword
--
-- The const keyword requires that the instance value is exactly equal to
-- the specified constant value (JSON Schema draft-06+).
module Fractal.JsonSchema.Keywords.Const
  ( constKeyword
    -- * Backward compatibility
  , validateConstConstraint
  ) where

import Data.Aeson (Value)
import Control.Monad.Reader (Reader)
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema, SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, validationFailure)

-- | Compiled data for the 'const' keyword
data ConstData = ConstData
  { constExpectedValue :: Value
  } deriving (Show, Eq, Typeable)

-- | Compile function for 'const' keyword
compileConst :: CompileFunc ConstData
compileConst value _schema _ctx = Right $ ConstData { constExpectedValue = value }

-- | Validate function for 'const' keyword
validateConst :: ValidateFunc ConstData
validateConst _recursiveValidator (ConstData expected) _ctx actual =
  if actual == expected
    then pure []
    else pure ["Value does not match const"]

-- | The 'const' keyword definition
constKeyword :: KeywordDefinition
constKeyword = KeywordDefinition
  { keywordName = "const"
  , keywordScope = AnyScope
  , keywordCompile = compileConst
  , keywordValidate = validateConst
  , keywordNavigation = NoNavigation
  }

-- | Backward compatibility: validate const constraint from SchemaObject
validateConstConstraint :: SchemaObject -> Value -> ValidationResult
validateConstConstraint obj val = case schemaConst obj of
  Nothing -> ValidationSuccess mempty
  Just expected ->
    if val == expected
      then ValidationSuccess mempty
      else validationFailure "const" "Value does not match const"
