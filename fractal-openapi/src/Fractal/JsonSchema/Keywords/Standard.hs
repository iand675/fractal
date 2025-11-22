{-# LANGUAGE OverloadedStrings #-}
-- | Standard JSON Schema keywords implemented using the pluggable keyword system
--
-- This module provides implementations of standard JSON Schema keywords
-- (const, enum, type, etc.) using the new compile-then-validate architecture.
-- These keywords are no longer privileged - they're implemented exactly like
-- custom keywords would be.
module Fractal.JsonSchema.Keywords.Standard
  ( -- * Standard Keywords
    constKeyword
  , enumKeyword
  , typeKeyword
  , minLengthKeyword
  , maxLengthKeyword
  , patternKeyword
  , minimumKeyword
  , maximumKeyword
  , multipleOfKeyword
    -- * Registry
  , standardKeywordRegistry
  ) where

import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword (KeywordRegistry, emptyKeywordRegistry, registerKeyword)
import Fractal.JsonSchema.Types
import qualified Fractal.JsonSchema.Regex as Regex
import Data.Aeson (Value(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import qualified Data.Scientific as Sci
import Data.Typeable (Typeable)
import Data.Foldable (toList)

-- | Compiled data for the 'const' keyword
data ConstData = ConstData
  { constExpectedValue :: Value
  } deriving (Show, Eq, Typeable)

-- | Compile function for 'const' keyword
compileConst :: CompileFunc ConstData
compileConst value _schema _ctx = Right $ ConstData { constExpectedValue = value }

-- | Validate function for 'const' keyword
validateConst :: ValidateFunc ConstData
validateConst (ConstData expected) actual =
  if actual == expected
    then []
    else ["Value does not match const"]

-- | The 'const' keyword definition
constKeyword :: KeywordDefinition
constKeyword = KeywordDefinition
  { keywordName = "const"
  , keywordScope = AnyScope
  , keywordCompile = compileConst
  , keywordValidate = validateConst
  }

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
  }

-- | Compiled data for the 'type' keyword
data TypeData = TypeData
  { typeExpectedTypes :: [SchemaType]
  } deriving (Show, Eq, Typeable)

-- | Compile function for 'type' keyword
compileType :: CompileFunc TypeData
compileType value _schema _ctx = case value of
  String typeStr -> case parseSchemaType typeStr of
    Just t -> Right $ TypeData { typeExpectedTypes = [t] }
    Nothing -> Left $ "Unknown type: " <> typeStr
  Array arr -> do
    types <- mapM parseTypeFromValue (toList arr)
    Right $ TypeData { typeExpectedTypes = types }
  _ -> Left "type must be a string or array of strings"
  where
    parseTypeFromValue (String s) = case parseSchemaType s of
      Just t -> Right t
      Nothing -> Left $ "Unknown type: " <> s
    parseTypeFromValue _ = Left "type array must contain only strings"

    parseSchemaType :: Text -> Maybe SchemaType
    parseSchemaType "null" = Just NullType
    parseSchemaType "boolean" = Just BooleanType
    parseSchemaType "string" = Just StringType
    parseSchemaType "number" = Just NumberType
    parseSchemaType "integer" = Just IntegerType
    parseSchemaType "object" = Just ObjectType
    parseSchemaType "array" = Just ArrayType
    parseSchemaType _ = Nothing

-- | Validate function for 'type' keyword
validateType :: ValidateFunc TypeData
validateType (TypeData expectedTypes) actual =
  if any (matchesType actual) expectedTypes
    then []
    else ["Expected one of: " <> T.intercalate ", " (map (T.pack . show) expectedTypes)]
  where
    matchesType :: Value -> SchemaType -> Bool
    matchesType Null NullType = True
    matchesType (Bool _) BooleanType = True
    matchesType (String _) StringType = True
    matchesType (Number _) NumberType = True
    matchesType (Number n) IntegerType = Sci.isInteger n
    matchesType (Object _) ObjectType = True
    matchesType (Array _) ArrayType = True
    matchesType _ _ = False

-- | The 'type' keyword definition
typeKeyword :: KeywordDefinition
typeKeyword = KeywordDefinition
  { keywordName = "type"
  , keywordScope = AnyScope
  , keywordCompile = compileType
  , keywordValidate = validateType
  }

-- ============================================================================
-- String Validation Keywords
-- ============================================================================

-- | Compiled data for the 'minLength' keyword
newtype MinLengthData = MinLengthData Int
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minLength' keyword
compileMinLength :: CompileFunc MinLengthData
compileMinLength value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 -> Right $ MinLengthData (truncate n)
  _ -> Left "minLength must be a non-negative integer"

-- | Validate function for 'minLength' keyword
validateMinLength :: ValidateFunc MinLengthData
validateMinLength (MinLengthData minLen) (String txt) =
  if T.length txt >= minLen
    then []
    else ["String length " <> T.pack (show (T.length txt)) <> " is less than minLength " <> T.pack (show minLen)]
validateMinLength _ _ = []  -- Only applies to strings

-- | The 'minLength' keyword definition
minLengthKeyword :: KeywordDefinition
minLengthKeyword = KeywordDefinition
  { keywordName = "minLength"
  , keywordScope = AnyScope
  , keywordCompile = compileMinLength
  , keywordValidate = validateMinLength
  }

-- | Compiled data for the 'maxLength' keyword
newtype MaxLengthData = MaxLengthData Int
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maxLength' keyword
compileMaxLength :: CompileFunc MaxLengthData
compileMaxLength value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 -> Right $ MaxLengthData (truncate n)
  _ -> Left "maxLength must be a non-negative integer"

-- | Validate function for 'maxLength' keyword
validateMaxLength :: ValidateFunc MaxLengthData
validateMaxLength (MaxLengthData maxLen) (String txt) =
  if T.length txt <= maxLen
    then []
    else ["String length " <> T.pack (show (T.length txt)) <> " exceeds maxLength " <> T.pack (show maxLen)]
validateMaxLength _ _ = []  -- Only applies to strings

-- | The 'maxLength' keyword definition
maxLengthKeyword :: KeywordDefinition
maxLengthKeyword = KeywordDefinition
  { keywordName = "maxLength"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxLength
  , keywordValidate = validateMaxLength
  }

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
validatePattern (PatternData regex patternStr) (String txt) =
  if Regex.matchRegex regex txt
    then []
    else ["String does not match pattern: " <> patternStr]
validatePattern _ _ = []  -- Only applies to strings

-- | The 'pattern' keyword definition
patternKeyword :: KeywordDefinition
patternKeyword = KeywordDefinition
  { keywordName = "pattern"
  , keywordScope = AnyScope
  , keywordCompile = compilePattern
  , keywordValidate = validatePattern
  }

-- ============================================================================
-- Numeric Validation Keywords
-- ============================================================================

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
  }

-- | Compiled data for the 'maximum' keyword
newtype MaximumData = MaximumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maximum' keyword
compileMaximum :: CompileFunc MaximumData
compileMaximum value _schema _ctx = case value of
  Number n -> Right $ MaximumData n
  _ -> Left "maximum must be a number"

-- | Validate function for 'maximum' keyword
validateMaximum :: ValidateFunc MaximumData
validateMaximum (MaximumData maxVal) (Number n) =
  if n <= maxVal
    then []
    else ["Value " <> T.pack (show n) <> " exceeds maximum " <> T.pack (show maxVal)]
validateMaximum _ _ = []  -- Only applies to numbers

-- | The 'maximum' keyword definition
maximumKeyword :: KeywordDefinition
maximumKeyword = KeywordDefinition
  { keywordName = "maximum"
  , keywordScope = AnyScope
  , keywordCompile = compileMaximum
  , keywordValidate = validateMaximum
  }

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

-- | Registry containing all standard keywords
--
-- This registry can be extended with custom keywords or used as-is
-- for standard JSON Schema validation.
standardKeywordRegistry :: KeywordRegistry
standardKeywordRegistry =
  -- Basic validation
  registerKeyword constKeyword $
  registerKeyword enumKeyword $
  registerKeyword typeKeyword $
  -- String validation
  registerKeyword minLengthKeyword $
  registerKeyword maxLengthKeyword $
  registerKeyword patternKeyword $
  -- Numeric validation
  registerKeyword minimumKeyword $
  registerKeyword maximumKeyword $
  registerKeyword multipleOfKeyword
  emptyKeywordRegistry
