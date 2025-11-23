{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'oneOf' composition keyword
--
-- The oneOf keyword requires that the instance validates against EXACTLY ONE
-- schema in the provided array. This is an applicator keyword that recursively
-- validates subschemas and collects annotations from the single passing branch.
module Fractal.JsonSchema.Keywords.OneOf
  ( validateOneOf
  , oneOfKeyword
  , compileOneOf
  , OneOfData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types (Schema(..), SchemaCore(..), SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure, schemaOneOf)
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, ValidationContext'(..), KeywordNavigation(..), KeywordScope(..))
import Fractal.JsonSchema.Keyword.Compile (compileKeyword)
import Fractal.JsonSchema.Parser (parseSchema, ParseError)

-- | Compiled data for oneOf keyword
newtype OneOfData = OneOfData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the oneOf keyword
compileOneOf :: CompileFunc OneOfData
compileOneOf value schema ctx = case value of
  Array arr | not (V.null arr) -> do
    -- Parse each element as a schema
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Just schemas' -> Right (OneOfData schemas')
      Nothing -> Left "oneOf must contain at least one schema"
  _ -> Left "oneOf must be a non-empty array"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in oneOf: " <> T.pack (show err)
      Right s -> Right s

-- | Validate oneOf using the pluggable keyword system
validateOneOfKeyword :: ValidateFunc OneOfData
validateOneOfKeyword recursiveValidator (OneOfData schemas) _ctx value =
  case validateOneOf recursiveValidator schemas value of
    ValidationSuccess _ -> pure []
    ValidationFailure errs -> pure [T.pack $ show errs]  -- TODO: proper error formatting

-- | Validate that a value satisfies EXACTLY ONE schema in oneOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas, exactly one must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with annotations from the single passing branch
-- - ValidationFailure if zero or more than one branch passes
validateOneOf
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas, exactly one must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateOneOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      successes = [anns | ValidationSuccess anns <- results]
  in case length successes of
    -- Collect annotations from the single passing branch
    1 -> ValidationSuccess $ head successes
    0 -> validationFailure "oneOf" "Value does not match any schema in oneOf"
    _ -> validationFailure "oneOf" "Value matches more than one schema in oneOf"

-- | Keyword definition for oneOf
oneOfKeyword :: KeywordDefinition
oneOfKeyword = KeywordDefinition
  { keywordName = "oneOf"
  , keywordScope = AnyScope
  , keywordCompile = compileOneOf
  , keywordValidate = validateOneOfKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> fmap NE.toList (schemaOneOf obj)
      _ -> Nothing
  }
