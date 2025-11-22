{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'enum' keyword
--
-- The enum keyword requires that the instance value is equal to one of
-- the elements in the specified array. Values are compared using JSON equality.
module Fractal.JsonSchema.Keywords.Enum
  ( enumKeyword
    -- * Backward compatibility
  , validateEnumConstraint
  ) where

import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE

import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordNavigation(..), CompileFunc, ValidateFunc, KeywordScope(..))
import Fractal.JsonSchema.Types (Schema, SchemaObject(..), ValidationResult(..), validationFailure)

-- | Compiled data for the 'enum' keyword
data EnumData = EnumData
  { enumAllowedValues :: [Value]
  } deriving (Show, Eq, Typeable)

-- | Compile function for 'enum' keyword
compileEnum :: CompileFunc EnumData
compileEnum value _schema _ctx = case value of
  Array arr -> Right $ EnumData { enumAllowedValues = toList arr }
  _ -> Left "enum must be an array"

-- | Validate function for 'enum' keyword
validateEnum :: ValidateFunc EnumData
validateEnum (EnumData allowedValues) actual =
  if actual `elem` allowedValues
    then []
    else ["Value not in enum: " <> T.pack (show actual)]

-- | The 'enum' keyword definition
enumKeyword :: KeywordDefinition
enumKeyword = KeywordDefinition
  { keywordName = "enum"
  , keywordScope = AnyScope
  , keywordCompile = compileEnum
  , keywordValidate = validateEnum
  , keywordNavigation = NoNavigation
  }

-- | Backward compatibility: validate enum constraint from SchemaObject
validateEnumConstraint :: SchemaObject -> Value -> ValidationResult
validateEnumConstraint obj val = case schemaEnum obj of
  Nothing -> ValidationSuccess mempty
  Just allowedValues ->
    if val `elem` NE.toList allowedValues
      then ValidationSuccess mempty
      else validationFailure "enum" $ "Value not in enum: " <> T.pack (show val)
