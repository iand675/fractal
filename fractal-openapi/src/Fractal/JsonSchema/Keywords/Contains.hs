{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-- | Implementation of the 'contains', 'minContains', and 'maxContains' keywords
--
-- The contains keyword requires that at least one array item validates against
-- the specified schema. By default, at least 1 item must match (minContains = 1).
-- minContains and maxContains can adjust this requirement.
module Fractal.JsonSchema.Keywords.Contains
  ( containsKeyword
  , minContainsKeyword
  , maxContainsKeyword
  , compileContains
  , ContainsData(..)
  ) where

import Data.Aeson (Value(..))
import Control.Monad.Reader (Reader)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import Data.Foldable (toList)
import qualified Data.Set as Set
import Numeric.Natural (Natural)
import qualified Data.Scientific as Sci
import Data.Maybe (fromMaybe)

import Fractal.JsonSchema.Types 
  ( Schema(..), SchemaCore(..), SchemaObject(..)
  , ValidationResult, validationFailure, pattern ValidationSuccess, pattern ValidationFailure
  , validationContains, validationMinContains, validationMaxContains, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..)
  )
import Fractal.JsonSchema.Parser.Internal (parseSchema)
import Fractal.JsonSchema.Validator.Annotations
  ( annotateItems
  , arrayIndexPointer
  , shiftAnnotations
  )

-- | Compiled data for contains keyword
data ContainsData = ContainsData 
  { containsSchema :: Schema
  , containsMinCount :: Natural  -- minContains value (default 1 when contains is present)
  , containsMaxCount :: Maybe Natural  -- maxContains value (if specified)
  }
  deriving (Typeable)

-- | Compiled data for minContains keyword
newtype MinContainsData = MinContainsData Natural
  deriving (Show, Eq, Typeable)

-- | Compiled data for maxContains keyword
newtype MaxContainsData = MaxContainsData Natural
  deriving (Show, Eq, Typeable)

-- | Compile the contains keyword
compileContains :: CompileFunc ContainsData
compileContains value schema _ctx = case parseSchema value of
  Left err -> Left $ "Invalid schema in contains: " <> T.pack (show err)
  Right containsSchema ->
    -- Read minContains and maxContains from adjacent keywords in the schema
    let validation = case schemaCore schema of
          ObjectSchema obj -> schemaValidation obj
          _ -> error "contains can only appear in object schemas"
        -- When contains is present, minContains defaults to 1
        minCount = maybe 1 fromIntegral (validationMinContains validation)
        maxCount = fmap fromIntegral (validationMaxContains validation)
    in Right $ ContainsData containsSchema minCount maxCount

-- | Compile the minContains keyword
compileMinContains :: CompileFunc MinContainsData
compileMinContains value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MinContainsData (fromInteger $ truncate n)
  _ -> Left "minContains must be a non-negative integer"

-- | Compile the maxContains keyword
compileMaxContains :: CompileFunc MaxContainsData
compileMaxContains value _schema _ctx = case value of
  Number n | Sci.isInteger n && n >= 0 ->
    Right $ MaxContainsData (fromInteger $ truncate n)
  _ -> Left "maxContains must be a non-negative integer"

-- | Validate contains using the pluggable keyword system
--
-- This keyword checks that at least minContains (default 1) items match
-- the schema, and at most maxContains items match (if maxContains is specified).
validateContainsKeyword :: ValidateFunc ContainsData
validateContainsKeyword recursiveValidator (ContainsData schema' minCount maxCount) _ctx (Array arr) =
  let evaluations =
        [ (idx, recursiveValidator schema' item)
        | (idx, item) <- zip [0..] (toList arr)
        ]
      matchingIndices = Set.fromList [idx | (idx, ValidationSuccess _) <- evaluations]
      matchCount = fromIntegral (Set.size matchingIndices) :: Natural
      minCheck = matchCount >= minCount
      maxCheck = maybe True (matchCount <=) maxCount
      shiftedAnnotations =
        [ shiftAnnotations (arrayIndexPointer idx) anns
        | (idx, ValidationSuccess anns) <- evaluations
        ]
  in pure $
    if not minCheck
      then validationFailure "contains" $
        "Array has " <> T.pack (show matchCount)
        <> " items matching contains, but minContains requires at least "
        <> T.pack (show minCount)
    else if not maxCheck
      then validationFailure "contains" $
        "Array has " <> T.pack (show matchCount)
        <> " items matching contains, but maxContains allows at most "
        <> maybe "0" (T.pack . show) maxCount
    else ValidationSuccess (annotateItems matchingIndices <> mconcat shiftedAnnotations)

validateContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Only applies to arrays

-- | Validate minContains (no-op, behavior is enforced by contains keyword)
validateMinContainsKeyword :: ValidateFunc MinContainsData
validateMinContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Behavior handled by contains keyword

-- | Validate maxContains (no-op, behavior is enforced by contains keyword)
validateMaxContainsKeyword :: ValidateFunc MaxContainsData
validateMaxContainsKeyword _ _ _ _ = pure (ValidationSuccess mempty)  -- Behavior handled by contains keyword

-- | Keyword definition for contains
containsKeyword :: KeywordDefinition
containsKeyword = KeywordDefinition
  { keywordName = "contains"
  , keywordCompile = compileContains
  , keywordValidate = validateContainsKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationContains (schemaValidation obj)
      _ -> Nothing
  , keywordPostValidate = Nothing
  }

-- | Keyword definition for minContains
minContainsKeyword :: KeywordDefinition
minContainsKeyword = KeywordDefinition
  { keywordName = "minContains"
  , keywordCompile = compileMinContains
  , keywordValidate = validateMinContainsKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

-- | Keyword definition for maxContains
maxContainsKeyword :: KeywordDefinition
maxContainsKeyword = KeywordDefinition
  { keywordName = "maxContains"
  , keywordCompile = compileMaxContains
  , keywordValidate = validateMaxContainsKeyword
  , keywordNavigation = NoNavigation
  , keywordPostValidate = Nothing
  }

