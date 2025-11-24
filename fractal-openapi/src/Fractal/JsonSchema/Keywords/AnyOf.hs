{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the 'anyOf' composition keyword
--
-- The anyOf keyword requires that the instance validates against AT LEAST ONE
-- schema in the provided array. This is an applicator keyword that recursively
-- validates subschemas and collects annotations from all passing branches.
module Fractal.JsonSchema.Keywords.AnyOf
  ( validateAnyOf
  , anyOfKeyword
  , compileAnyOf
  , AnyOfData(..)
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

import Fractal.JsonSchema.Types (Schema(..), SchemaCore(..), SchemaObject(..), ValidationResult, pattern ValidationSuccess, pattern ValidationFailure, ValidationAnnotations, validationFailure, schemaAnyOf)
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), CompileFunc, ValidateFunc, ValidationContext'(..), KeywordNavigation(..), CompilationContext(..))
import Fractal.JsonSchema.Keyword.Compile (compileKeyword)
import Fractal.JsonSchema.Parser.Internal (parseSchema)
import Fractal.JsonSchema.Types (ParseError)

-- | Compiled data for anyOf keyword
newtype AnyOfData = AnyOfData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the anyOf keyword
compileAnyOf :: CompileFunc AnyOfData
compileAnyOf value schema ctx =
  case schemaCore schema of
    ObjectSchema obj ->
      case schemaAnyOf obj of
        Just schemas -> Right (AnyOfData schemas)
        Nothing -> parseFromValue
    _ -> parseFromValue
  where
    parseFromValue = case value of
      Array arr | not (V.null arr) -> do
        parsedSchemas <- mapM parseSchemaElem (V.toList arr)
        case NE.nonEmpty parsedSchemas of
          Just schemas' -> Right (AnyOfData schemas')
          Nothing -> Left "anyOf must contain at least one schema"
      _ -> Left "anyOf must be a non-empty array"

    parseSchemaElem v = case contextParseSubschema ctx v of
      Left err -> Left $ "Invalid schema in anyOf: " <> err
      Right s -> Right s

-- | Validate anyOf using the pluggable keyword system
validateAnyOfKeyword :: ValidateFunc AnyOfData
validateAnyOfKeyword recursiveValidator (AnyOfData schemas) _ctx value =
  pure $ validateAnyOf recursiveValidator schemas value

-- | Validate that a value satisfies AT LEAST ONE schema in anyOf
--
-- Parameters:
-- - validateSchema: Recursive validation function for subschemas
-- - schemas: The list of schemas, at least one must validate
-- - value: The instance value to validate
--
-- Returns:
-- - ValidationSuccess with combined annotations from ALL passing branches
-- - ValidationFailure if NO branches pass
validateAnyOf
  :: (Schema -> Value -> ValidationResult)  -- ^ Recursive validator
  -> NonEmpty Schema                        -- ^ Schemas, at least one must validate
  -> Value                                   -- ^ Value to validate
  -> ValidationResult
validateAnyOf validateSchema schemas value =
  let results = [validateSchema schema value | schema <- NE.toList schemas]
      successes = [anns | ValidationSuccess anns <- results]
  in if null successes
    then validationFailure "anyOf" "Value does not match any schema in anyOf"
    -- Collect annotations from ALL passing branches in anyOf
    else ValidationSuccess $ mconcat successes

-- | Keyword definition for anyOf
anyOfKeyword :: KeywordDefinition
anyOfKeyword = KeywordDefinition
  { keywordName = "anyOf"
  , keywordCompile = compileAnyOf
  , keywordValidate = validateAnyOfKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> fmap NE.toList (schemaAnyOf obj)
      _ -> Nothing
  , keywordPostValidate = Nothing
  }
