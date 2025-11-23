{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'items' keyword
--
-- The items keyword validates array items. It has two modes:
-- 1. Schema mode: A single schema that applies to all array items
-- 2. Tuple mode (Draft-04 only): An array of schemas for positional validation
--
-- In Draft 2020-12+, tuple validation uses prefixItems instead of items.
module Fractal.JsonSchema.Keywords.Items
  ( itemsKeyword
  , compileItems
  , ItemsData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
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
  , ArrayItemsValidation(..), validationItems, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  )
import Fractal.JsonSchema.Parser (parseSchema)

-- | Compiled data for items keyword
data ItemsData = ItemsData ArrayItemsValidation
  deriving (Typeable)

-- | Compile the items keyword
compileItems :: CompileFunc ItemsData
compileItems value _schema _ctx = case value of
  Object _ -> do
    -- Single schema for all items
    case parseSchema value of
      Left err -> Left $ "Invalid schema in items: " <> T.pack (show err)
      Right itemSchema -> Right $ ItemsData (ItemsSchema itemSchema)
  
  Bool b -> do
    -- Boolean schema
    case parseSchema (Bool b) of
      Left err -> Left $ "Invalid boolean schema in items: " <> T.pack (show err)
      Right boolSchema -> Right $ ItemsData (ItemsSchema boolSchema)
  
  Array arr | not (V.null arr) -> do
    -- Tuple validation (Draft-04 style)
    -- Each element is a schema for the corresponding array position
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Just schemas' -> Right $ ItemsData (ItemsTuple schemas' Nothing)
      Nothing -> Left "items array must contain at least one schema"
  
  _ -> Left "items must be a schema, boolean, or non-empty array of schemas"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in items array: " <> T.pack (show err)
      Right s -> Right s

-- | Validate items using the pluggable keyword system
validateItemsKeyword :: ValidateFunc ItemsData
validateItemsKeyword recursiveValidator (ItemsData itemsValidation) _ctx (Array arr) =
  case itemsValidation of
    ItemsSchema itemSchema ->
      -- All items must validate against the schema
      let results = map (recursiveValidator itemSchema) (toList arr)
          failures = [errs | ValidationFailure errs <- results]
      in case failures of
        [] -> pure []  -- Success
        _ -> pure [T.pack $ show err | err <- failures]
    
    ItemsTuple tupleSchemas maybeAdditional ->
      -- Positional validation + optional additional items
      let tupleResults = zipWith (recursiveValidator) (NE.toList tupleSchemas) (toList arr)
          tupleFailures = [errs | ValidationFailure errs <- tupleResults]
          
          -- Handle additional items beyond tuple length
          additionalItems = drop (length tupleSchemas) (toList arr)
          
          -- Validate additional items if schema provided
          additionalResults = case maybeAdditional of
            Just addlSchema -> map (recursiveValidator addlSchema) additionalItems
            Nothing -> []  -- additionalItems not specified - allow them
          
          additionalFailures = [errs | ValidationFailure errs <- additionalResults]
          allFailures = tupleFailures <> additionalFailures
      in case allFailures of
        [] -> pure []  -- Success
        _ -> pure [T.pack $ show err | err <- allFailures]

validateItemsKeyword _ _ _ _ = pure []  -- Only applies to arrays

-- | Keyword definition for items
itemsKeyword :: KeywordDefinition
itemsKeyword = KeywordDefinition
  { keywordName = "items"
  , keywordScope = AnyScope
  , keywordCompile = compileItems
  , keywordValidate = validateItemsKeyword
  , keywordNavigation = CustomNavigation $ \schema _seg rest -> case schemaCore schema of
      ObjectSchema obj ->
        case validationItems (schemaValidation obj) of
          Just (ItemsSchema itemSchema) ->
            -- Single schema for all items - return it with full rest path
            Just (itemSchema, rest)
          Just (ItemsTuple schemas _) ->
            -- Array of schemas - need index from rest
            case rest of
              (idx:remaining) ->
                case reads (T.unpack idx) :: [(Int, String)] of
                  [(n, "")] | n >= 0 && n < length schemas ->
                    Just (NE.toList schemas !! n, remaining)
                  _ -> Nothing
              [] -> Nothing  -- Need an index for tuple items
          Nothing -> Nothing
      _ -> Nothing
  }

