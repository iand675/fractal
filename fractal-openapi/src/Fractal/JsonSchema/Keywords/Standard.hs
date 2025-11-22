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
  , exclusiveMinimumKeyword
  , exclusiveMaximumKeyword
    -- * Array Keywords
  , minItemsKeyword
  , maxItemsKeyword
  , uniqueItemsKeyword
    -- * Object Keywords
  , requiredKeyword
  , minPropertiesKeyword
  , maxPropertiesKeyword
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
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key

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

-- | Compiled data for the 'exclusiveMinimum' keyword (Draft-06+ standalone numeric)
newtype ExclusiveMinimumData = ExclusiveMinimumData Sci.Scientific
  deriving (Show, Eq, Typeable)

-- | Compile function for 'exclusiveMinimum' keyword
compileExclusiveMinimum :: CompileFunc ExclusiveMinimumData
compileExclusiveMinimum value _schema _ctx = case value of
  Number n -> Right $ ExclusiveMinimumData n
  _ -> Left "exclusiveMinimum must be a number"

-- | Validate function for 'exclusiveMinimum' keyword
validateExclusiveMinimum :: ValidateFunc ExclusiveMinimumData
validateExclusiveMinimum (ExclusiveMinimumData minVal) (Number n) =
  if n > minVal
    then []
    else ["Value " <> T.pack (show n) <> " must be greater than exclusiveMinimum " <> T.pack (show minVal)]
validateExclusiveMinimum _ _ = []  -- Only applies to numbers

-- | The 'exclusiveMinimum' keyword definition (Draft-06+ style)
exclusiveMinimumKeyword :: KeywordDefinition
exclusiveMinimumKeyword = KeywordDefinition
  { keywordName = "exclusiveMinimum"
  , keywordScope = AnyScope
  , keywordCompile = compileExclusiveMinimum
  , keywordValidate = validateExclusiveMinimum
  }

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

-- ============================================================================
-- Array Validation Keywords
-- ============================================================================

-- | Compiled data for the 'minItems' keyword
newtype MinItemsData = MinItemsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minItems' keyword
compileMinItems :: CompileFunc MinItemsData
compileMinItems value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinItemsData (fromInteger $ truncate n)
  _ -> Left "minItems must be a non-negative integer"

-- | Validate function for 'minItems' keyword
validateMinItems :: ValidateFunc MinItemsData
validateMinItems (MinItemsData minLen) (Array arr) =
  let arrLength = fromIntegral (length arr) :: Natural
  in if arrLength >= minLen
     then []
     else ["Array length " <> T.pack (show arrLength) <> " is less than minItems " <> T.pack (show minLen)]
validateMinItems _ _ = []  -- Only applies to arrays

-- | The 'minItems' keyword definition
minItemsKeyword :: KeywordDefinition
minItemsKeyword = KeywordDefinition
  { keywordName = "minItems"
  , keywordScope = AnyScope
  , keywordCompile = compileMinItems
  , keywordValidate = validateMinItems
  }

-- | Compiled data for the 'maxItems' keyword
newtype MaxItemsData = MaxItemsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'maxItems' keyword
compileMaxItems :: CompileFunc MaxItemsData
compileMaxItems value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MaxItemsData (fromInteger $ truncate n)
  _ -> Left "maxItems must be a non-negative integer"

-- | Validate function for 'maxItems' keyword
validateMaxItems :: ValidateFunc MaxItemsData
validateMaxItems (MaxItemsData maxLen) (Array arr) =
  let arrLength = fromIntegral (length arr) :: Natural
  in if arrLength <= maxLen
     then []
     else ["Array length " <> T.pack (show arrLength) <> " exceeds maxItems " <> T.pack (show maxLen)]
validateMaxItems _ _ = []  -- Only applies to arrays

-- | The 'maxItems' keyword definition
maxItemsKeyword :: KeywordDefinition
maxItemsKeyword = KeywordDefinition
  { keywordName = "maxItems"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxItems
  , keywordValidate = validateMaxItems
  }

-- | Compiled data for the 'uniqueItems' keyword
newtype UniqueItemsData = UniqueItemsData Bool
  deriving (Show, Eq, Typeable)

-- | Compile function for 'uniqueItems' keyword
compileUniqueItems :: CompileFunc UniqueItemsData
compileUniqueItems value _schema _ctx = case value of
  Bool b -> Right $ UniqueItemsData b
  _ -> Left "uniqueItems must be a boolean"

-- | Validate function for 'uniqueItems' keyword
validateUniqueItems :: ValidateFunc UniqueItemsData
validateUniqueItems (UniqueItemsData True) (Array arr) =
  let items = toList arr
      uniqueItems = length items == length (nubOrd items)
  in if uniqueItems
     then []
     else ["Array contains duplicate items"]
  where
    -- Simple deduplication using Ord (works for most JSON values)
    nubOrd :: Ord a => [a] -> [a]
    nubOrd = Set.toList . Set.fromList
validateUniqueItems (UniqueItemsData False) _ = []  -- uniqueItems: false means no constraint
validateUniqueItems _ _ = []  -- Only applies to arrays when true

-- | The 'uniqueItems' keyword definition
uniqueItemsKeyword :: KeywordDefinition
uniqueItemsKeyword = KeywordDefinition
  { keywordName = "uniqueItems"
  , keywordScope = AnyScope
  , keywordCompile = compileUniqueItems
  , keywordValidate = validateUniqueItems
  }

-- ============================================================================
-- Object Validation Keywords
-- ============================================================================

-- | Compiled data for the 'required' keyword
newtype RequiredData = RequiredData (Set.Set Text)
  deriving (Show, Eq, Typeable)

-- | Compile function for 'required' keyword
compileRequired :: CompileFunc RequiredData
compileRequired value _schema _ctx = case value of
  Array arr -> do
    let items = toList arr
    -- All items must be strings
    strings <- mapM extractString items
    Right $ RequiredData (Set.fromList strings)
  _ -> Left "required must be an array of strings"
  where
    extractString (String s) = Right s
    extractString _ = Left "required array must contain only strings"

-- | Validate function for 'required' keyword
validateRequired :: ValidateFunc RequiredData
validateRequired (RequiredData requiredProps) (Object objMap) =
  let presentProps = Set.fromList [Key.toText k | k <- KeyMap.keys objMap]
      missingProps = Set.difference requiredProps presentProps
  in if Set.null missingProps
     then []
     else ["Missing required properties: " <> T.intercalate ", " (Set.toList missingProps)]
validateRequired _ _ = []  -- Only applies to objects

-- | The 'required' keyword definition
requiredKeyword :: KeywordDefinition
requiredKeyword = KeywordDefinition
  { keywordName = "required"
  , keywordScope = AnyScope
  , keywordCompile = compileRequired
  , keywordValidate = validateRequired
  }

-- | Compiled data for the 'minProperties' keyword
newtype MinPropertiesData = MinPropertiesData Natural
  deriving (Show, Eq, Typeable)

-- | Compile function for 'minProperties' keyword
compileMinProperties :: CompileFunc MinPropertiesData
compileMinProperties value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinPropertiesData (fromInteger $ truncate n)
  _ -> Left "minProperties must be a non-negative integer"

-- | Validate function for 'minProperties' keyword
validateMinProperties :: ValidateFunc MinPropertiesData
validateMinProperties (MinPropertiesData minProps) (Object objMap) =
  let propCount = fromIntegral (KeyMap.size objMap) :: Natural
  in if propCount >= minProps
     then []
     else ["Object has " <> T.pack (show propCount) <> " properties, but minProperties is " <> T.pack (show minProps)]
validateMinProperties _ _ = []  -- Only applies to objects

-- | The 'minProperties' keyword definition
minPropertiesKeyword :: KeywordDefinition
minPropertiesKeyword = KeywordDefinition
  { keywordName = "minProperties"
  , keywordScope = AnyScope
  , keywordCompile = compileMinProperties
  , keywordValidate = validateMinProperties
  }

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
validateMaxProperties (MaxPropertiesData maxProps) (Object objMap) =
  let propCount = fromIntegral (KeyMap.size objMap) :: Natural
  in if propCount <= maxProps
     then []
     else ["Object has " <> T.pack (show propCount) <> " properties, but maxProperties is " <> T.pack (show maxProps)]
validateMaxProperties _ _ = []  -- Only applies to objects

-- | The 'maxProperties' keyword definition
maxPropertiesKeyword :: KeywordDefinition
maxPropertiesKeyword = KeywordDefinition
  { keywordName = "maxProperties"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxProperties
  , keywordValidate = validateMaxProperties
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
  registerKeyword multipleOfKeyword $
  registerKeyword exclusiveMinimumKeyword $
  registerKeyword exclusiveMaximumKeyword $
  -- Array validation
  registerKeyword minItemsKeyword $
  registerKeyword maxItemsKeyword $
  registerKeyword uniqueItemsKeyword $
  -- Object validation
  registerKeyword requiredKeyword $
  registerKeyword minPropertiesKeyword $
  registerKeyword maxPropertiesKeyword
  emptyKeywordRegistry
