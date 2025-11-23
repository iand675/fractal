{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'allOf' composition keyword
--
-- The allOf keyword requires that the instance validates against ALL schemas
-- in the provided array. This is an applicator keyword that recursively 
-- validates subschemas and collects annotations from all branches.
module Fractal.JsonSchema.Keywords.AllOf
  ( validateAllOf
  , allOfKeyword
  , compileAllOf
  , AllOfData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Types (Schema(..), SchemaCore(..), SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationErrors, ValidationAnnotations, schemaAllOf)
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, ValidationContext'(..), KeywordNavigation(..), KeywordScope(..))
import Fractal.JsonSchema.Keyword.Compile (compileKeyword)
import Fractal.JsonSchema.Parser (parseSchema, ParseError)

-- | Compiled data for allOf keyword
newtype AllOfData = AllOfData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the allOf keyword
compileAllOf :: CompileFunc AllOfData
compileAllOf value schema ctx = case value of
  Array arr | not (V.null arr) -> do
    -- Parse each element as a schema
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Just schemas' -> Right (AllOfData schemas')
      Nothing -> Left "allOf must contain at least one schema"
  _ -> Left "allOf must be a non-empty array"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in allOf: " <> T.pack (show err)
      Right s -> Right s

-- | Validate allOf using the pluggable keyword system
validateAllOfKeyword :: ValidateFunc AllOfData
validateAllOfKeyword recursiveValidator (AllOfData schemas) _ctx value =
  case validateAllOf recursiveValidator schemas value of
    ValidationSuccess _ -> []
    ValidationFailure errs -> validationErrorsToTexts errs
  where
    validationErrorsToTexts errs = [T.pack $ show errs]  -- TODO: proper error formatting

-- | Validate that a value satisfies ALL schemas in allOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas that all must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from all branches (all must pass)
-- - ValidationFailure with combined errors if any branch fails
validateAllOf 
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas that all must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAllOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      failures = [errs | ValidationFailure errs <- results]
      annotations = [anns | ValidationSuccess anns <- results]
  in case failures of
    -- Collect annotations from ALL branches in allOf (all must pass)
    [] -> ValidationSuccess $ mconcat annotations
    (e:es) -> ValidationFailure $ foldl (<>) e es

-- | Keyword definition for allOf
allOfKeyword :: KeywordDefinition
allOfKeyword = KeywordDefinition
  { keywordName = "allOf"
  , keywordScope = AnyScope
  , keywordCompile = compileAllOf
  , keywordValidate = validateAllOfKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> fmap NE.toList (schemaAllOf obj)
      _ -> Nothing
  }
