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
import qualified Data.Vector as V
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import Control.Monad.Reader (ask)
import Data.Set (Set)
import qualified Data.Set as Set

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , JsonSchemaVersion(..), ValidationConfig(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , ArrayItemsValidation(..), validationItems, validationPrefixItems, schemaValidation, schemaCore
  , ValidationAnnotations(..)
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  , CompilationContext(..), contextParseSubschema
  , combineValidationResults
  )
import Fractal.JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for items keyword
data ItemsData = ItemsData
  { itemsValidationData :: ArrayItemsValidation
  , itemsHasPrefix :: Bool
  }
  deriving (Typeable)

-- | Compile the items keyword
compileItems :: CompileFunc ItemsData
compileItems _value schema _ctx = 
  -- Extract the already-parsed items schemas from the parent SchemaObject
  -- This preserves base URI context that was set during initial schema parsing
  case schemaCore schema of
    BooleanSchema _ -> 
      Left "items keyword cannot appear in boolean schema"
    ObjectSchema obj ->
      case validationItems (schemaValidation obj) of
        Nothing -> Left "items keyword present but no parsed items found in schema"
        Just itemsValidation ->
          let hasPrefix = validationPrefixItems (schemaValidation obj) /= Nothing
          in Right $ ItemsData itemsValidation hasPrefix

-- | Validate items using the pluggable keyword system
validateItemsKeyword :: ValidateFunc ItemsData
validateItemsKeyword recursiveValidator (ItemsData itemsValidation hasPrefix) _ctx (Array arr) = do
  config <- ask
  let version = validationVersion config
  if hasPrefix && version >= Draft202012
    then pure (ValidationSuccess mempty)
    else case itemsValidation of
      ItemsSchema itemSchema ->
        let evaluations =
              [ (idx, recursiveValidator itemSchema item)
              | (idx, item) <- zip [0..] (toList arr)
              ]
            indices = Set.fromList [idx | (idx, _) <- evaluations]
            failures =
              [ errs
              | (_, ValidationFailure errs) <- evaluations
              ]
            shiftedAnnotations =
              [ shiftAnnotations (arrayIndexPointer idx) anns
              | (idx, ValidationSuccess anns) <- evaluations
              ]
        in pure $
             case failures of
               [] -> ValidationSuccess (annotateItems indices <> mconcat shiftedAnnotations)
               (e:es) -> ValidationFailure (foldl (<>) e es)
      ItemsTuple tupleSchemas maybeAdditional ->
        let tupleEvaluations =
              [ (idx, recursiveValidator schema item)
              | (idx, (schema, item)) <-
                  zip [0..] (zip (NE.toList tupleSchemas) (toList arr))
              ]
            tupleIndices = Set.fromList [idx | (idx, _) <- tupleEvaluations]
            additionalItems = drop (length tupleSchemas) (toList arr)
            additionalEvaluations = case maybeAdditional of
              Just addlSchema ->
                [ (idx, recursiveValidator addlSchema item)
                | (idx, item) <- zip [length tupleSchemas ..] additionalItems
                ]
              Nothing -> []
            additionalIndices = Set.fromList [idx | (idx, _) <- additionalEvaluations]
            failures =
              [ errs
              | (_, ValidationFailure errs) <- tupleEvaluations ++ additionalEvaluations
              ]
            shiftedAnnotations =
              [ shiftAnnotations (arrayIndexPointer idx) anns
              | (idx, ValidationSuccess anns) <- tupleEvaluations ++ additionalEvaluations
              ]
            allIndices = tupleIndices <> additionalIndices
        in pure $
             case failures of
               [] -> ValidationSuccess (annotateItems allIndices <> mconcat shiftedAnnotations)
               (e:es) -> ValidationFailure (foldl (<>) e es)


validateItemsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

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
  , keywordPostValidate = Nothing
  }

