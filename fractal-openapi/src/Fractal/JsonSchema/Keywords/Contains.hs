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
  , ValidationResult, pattern ValidationSuccess, pattern ValidationFailure
  , validationContains, validationMinContains, validationMaxContains, schemaValidation
  )
import Fractal.JsonSchema.Keyword.Types 
  ( KeywordDefinition(..), CompileFunc, ValidateFunc
  , ValidationContext'(..), KeywordNavigation(..), KeywordScope(..)
  )
import Fractal.JsonSchema.Parser (parseSchema)

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
  let results = [recursiveValidator schema' item | item <- toList arr]
      matchCount = fromIntegral (length [() | ValidationSuccess _ <- results]) :: Natural
      
      minCheck = matchCount >= minCount
      maxCheck = maybe True (matchCount <=) maxCount
  in if not minCheck
    then pure ["Array has " <> T.pack (show matchCount) <> " items matching contains, but minContains requires at least " <> T.pack (show minCount)]
    else if not maxCheck
      then pure ["Array has " <> T.pack (show matchCount) <> " items matching contains, but maxContains allows at most " <> T.pack (show (fromMaybe 0 maxCount))]
      else pure []  -- Success

validateContainsKeyword _ _ _ _ = pure []  -- Only applies to arrays

-- | Validate minContains (no-op, behavior is enforced by contains keyword)
validateMinContainsKeyword :: ValidateFunc MinContainsData
validateMinContainsKeyword _ _ _ _ = pure []  -- Behavior is handled by contains keyword

-- | Validate maxContains (no-op, behavior is enforced by contains keyword)
validateMaxContainsKeyword :: ValidateFunc MaxContainsData
validateMaxContainsKeyword _ _ _ _ = pure []  -- Behavior is handled by contains keyword

-- | Keyword definition for contains
containsKeyword :: KeywordDefinition
containsKeyword = KeywordDefinition
  { keywordName = "contains"
  , keywordScope = AnyScope
  , keywordCompile = compileContains
  , keywordValidate = validateContainsKeyword
  , keywordNavigation = SingleSchema $ \schema -> case schemaCore schema of
      ObjectSchema obj -> validationContains (schemaValidation obj)
      _ -> Nothing
  }

-- | Keyword definition for minContains
minContainsKeyword :: KeywordDefinition
minContainsKeyword = KeywordDefinition
  { keywordName = "minContains"
  , keywordScope = AnyScope
  , keywordCompile = compileMinContains
  , keywordValidate = validateMinContainsKeyword
  , keywordNavigation = NoNavigation
  }

-- | Keyword definition for maxContains
maxContainsKeyword :: KeywordDefinition
maxContainsKeyword = KeywordDefinition
  { keywordName = "maxContains"
  , keywordScope = AnyScope
  , keywordCompile = compileMaxContains
  , keywordValidate = validateMaxContainsKeyword
  , keywordNavigation = NoNavigation
  }

