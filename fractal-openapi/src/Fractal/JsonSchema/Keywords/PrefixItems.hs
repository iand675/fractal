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
import Data.Typeable (Typeable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.Set (Set)
import qualified Data.Set as Set

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..), ArrayItemsValidation(..)
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationPrefixItems, validationItems, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , KeywordNavigation(..)
  )
import Fractal.JsonSchema.Parser.Internal (parseSchema)
import Fractal.JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for prefixItems keyword
data PrefixItemsData = PrefixItemsData
  { prefixSchemas :: NonEmpty Schema
  , prefixAdditional :: Maybe ArrayItemsValidation
  }
  deriving (Typeable)

-- | Compile the prefixItems keyword
compilePrefixItems :: CompileFunc PrefixItemsData
compilePrefixItems value schema _ctx = case value of
  Array arr | not (V.null arr) -> do
    parsedSchemas <- mapM parseSchemaElem (V.toList arr)
    case NE.nonEmpty parsedSchemas of
      Nothing -> Left "prefixItems must contain at least one schema"
      Just schemas' ->
        case schemaCore schema of
          BooleanSchema _ -> Left "prefixItems cannot appear in boolean schemas"
          ObjectSchema obj ->
            Right $ PrefixItemsData schemas' (validationItems (schemaValidation obj))
  _ -> Left "prefixItems must be a non-empty array of schemas"
  where
    parseSchemaElem v = case parseSchema v of
      Left err -> Left $ "Invalid schema in prefixItems: " <> T.pack (show err)
      Right s -> Right s

-- | Validate prefixItems using the pluggable keyword system
validatePrefixItemsKeyword :: ValidateFunc PrefixItemsData
validatePrefixItemsKeyword recursiveValidator (PrefixItemsData schemas additional) _ctx (Array arr) =
  let prefixEvaluations =
        [ (idx, recursiveValidator schema item)
        | (idx, (schema, item)) <- zip [0..] (zip (NE.toList schemas) (toList arr))
        ]
      prefixFailures = [errs | (_, ValidationFailure errs) <- prefixEvaluations]
      prefixIndices = Set.fromList [idx | (idx, _) <- prefixEvaluations]
      prefixAnnotations =
        [ shiftAnnotations (arrayIndexPointer idx) anns
        | (idx, ValidationSuccess anns) <- prefixEvaluations
        ]
      startIdx = length schemas
      remainingItems = drop startIdx (toList arr)
      (additionalFailures, additionalIndices, additionalAnnotations) =
        case additional of
          Just (ItemsSchema itemSchema) ->
            let evals =
                  [ (idx, recursiveValidator itemSchema item)
                  | (idx, item) <- zip [startIdx..] remainingItems
                  ]
                failures = [errs | (_, ValidationFailure errs) <- evals]
                indices = if null evals then Set.empty else Set.fromList [startIdx .. startIdx + length evals - 1]
                annotations =
                  [ shiftAnnotations (arrayIndexPointer idx) anns
                  | (idx, ValidationSuccess anns) <- evals
                  ]
            in (failures, indices, annotations)
          Just (ItemsTuple tupleSchemas maybeAdditional) ->
            -- Treat tuple-style additional validation similar to legacy semantics
            let tupleEvals =
                  [ (idx, recursiveValidator schema' item)
                  | (idx, (schema', item)) <- zip [startIdx..] (zip (NE.toList tupleSchemas) remainingItems)
                  ]
                tupleFailures = [errs | (_, ValidationFailure errs) <- tupleEvals]
                tupleIndices = Set.fromList [idx | (idx, _) <- tupleEvals]
                tupleAnnotations =
                  [ shiftAnnotations (arrayIndexPointer idx) anns
                  | (idx, ValidationSuccess anns) <- tupleEvals
                  ]
                extraItems = drop (length tupleSchemas) remainingItems
                extraStart = startIdx + length tupleSchemas
                extraEvals = case maybeAdditional of
                  Just addlSchema ->
                    [ (idx, recursiveValidator addlSchema item)
                    | (idx, item) <- zip [extraStart..] extraItems
                    ]
                  Nothing -> []
                extraFailures = [errs | (_, ValidationFailure errs) <- extraEvals]
                extraIndices = if null extraEvals then Set.empty else Set.fromList [extraStart .. extraStart + length extraEvals - 1]
                extraAnnotations =
                  [ shiftAnnotations (arrayIndexPointer idx) anns
                  | (idx, ValidationSuccess anns) <- extraEvals
                  ]
            in (tupleFailures <> extraFailures, tupleIndices <> extraIndices, tupleAnnotations <> extraAnnotations)
          Nothing -> ([], Set.empty, [])
      allFailures = prefixFailures <> additionalFailures
      allIndices = prefixIndices <> additionalIndices
      allAnnotations = prefixAnnotations <> additionalAnnotations
  in pure $
       case allFailures of
         [] -> ValidationSuccess (annotateItems allIndices <> mconcat allAnnotations)
         (e:es) -> ValidationFailure (foldl (<>) e es)

validatePrefixItemsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Keyword definition for prefixItems
prefixItemsKeyword :: KeywordDefinition
prefixItemsKeyword = KeywordDefinition
  { keywordName = "prefixItems"
  , keywordCompile = compilePrefixItems
  , keywordValidate = validatePrefixItemsKeyword
  , keywordNavigation = SchemaArray $ \schema -> case schemaCore schema of
      ObjectSchema obj -> case validationPrefixItems (schemaValidation obj) of
        Just prefixSchemas -> Just (NE.toList prefixSchemas)
        Nothing -> Nothing
      _ -> Nothing
  , keywordPostValidate = Nothing
  }

