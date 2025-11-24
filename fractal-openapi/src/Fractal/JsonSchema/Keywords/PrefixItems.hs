{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'prefixItems' keyword (Draft 2020-12+)
--
-- The prefixItems keyword validates array items positionally. Each schema
-- in the prefixItems array validates the corresponding item at that index.
-- Items beyond the prefixItems array length are validated by the 'items' keyword.
module Fractal.JsonSchema.Keywords.PrefixItems
  ( prefixItemsKeyword
  , compilePrefixItems
  , PrefixItemsData(..)
  ) where

import Data.Aeson (Value(..))
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPrefixItems, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  , combineValidationResults
  )
import Fractal.JsonSchema.Parser (parseSchema)

-- | Compiled data for prefixItems keyword
newtype PrefixItemsData = PrefixItemsData (NonEmpty Schema)
  deriving (Typeable)

-- | Compile the prefixItems keyword
compilePrefixItems :: CompileFunc PrefixItemsData
compilePrefixItems value _schema _ctx = case value of
  Array arr | not (V.null arr) -> do
    -- Each element is a schema for the corresponding array position
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Just schemas' -> Right $ PrefixItemsData schemas'
      Nothing -> Left "prefixItems must contain at least one schema"
  
  _ -> Left "prefixItems must be a non-empty array of schemas"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in prefixItems: " <> T.pack (show err)
      Right s -> Right s

-- | Validate prefixItems using the pluggable keyword system
validatePrefixItemsKeyword :: ValidateFunc PrefixItemsData
validatePrefixItemsKeyword recursiveValidator (PrefixItemsData prefixSchemas) _ctx (Array arr) =
  -- Validate each item against its corresponding schema
  let results = zipWith recursiveValidator (NE.toList prefixSchemas) (toList arr)
  in pure $ combineValidationResults results

validatePrefixItemsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Keyword definition for prefixItems
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = KeywordDefinition
  { keywordName = "prefixItems"
  , keywordScope = AnyScope
  , keywordCompile = compilePrefixItems
  , keywordValidate = validatePrefixItemsKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> case validationPrefixItems (schemaValidation obj) of
        Just prefixSchemas -> Just (NE.toList prefixSchemas)
        Nothing -> Nothing
      _ -> Nothing
  }

