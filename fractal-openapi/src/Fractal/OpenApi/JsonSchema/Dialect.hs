-- | JSON Schema vocabulary and dialect system
--
-- This module provides the vocabulary and dialect system for extensible
-- JSON Schema support, including custom keyword definitions.
module Fractal.OpenApi.JsonSchema.Dialect
  ( -- * Vocabularies
    Vocabulary(..)
  , KeywordDefinition(..)
  , KeywordScope(..)
  , KeywordValue(..)

  -- * Dialects
  , Dialect(..)
  , UnknownKeywordMode(..)
  , FormatBehavior(..)

  -- * Registry
  , VocabularyRegistry(..)
  , emptyRegistry
  , registerVocabulary
  , registerDialect
  , lookupVocabulary
  , lookupDialect

  -- * Composition
  , composeDialect
  , composeVocabularies
  , extendVocabulary
  , variantVocabulary

  -- * Standard Dialects
  , draft04Dialect
  , draft06Dialect
  , draft07Dialect
  , draft201909Dialect
  , draft202012Dialect
  ) where

import Data.Aeson (Value)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Typeable (Typeable)
import Fractal.OpenApi.JsonSchema.Types (Schema, JSONPointer)

-- | Vocabulary definition
data Vocabulary = Vocabulary
  { vocabularyURI :: Text
  , vocabularyRequired :: Bool
  , vocabularyKeywords :: Map Text KeywordDefinition
  , vocabularyMetaSchema :: Maybe Schema
  } deriving (Eq, Show)

-- | Keyword definition
data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordAppliesTo :: KeywordScope
  , keywordPriority :: Int
  } deriving (Eq, Show)

-- | Keyword scope
data KeywordScope
  = AnySchema
  | ObjectSchemaOnly
  | ArraySchemaOnly
  | StringSchemaOnly
  | NumericSchemaOnly
  | BooleanSchemaOnly
  deriving (Eq, Show, Ord)

-- | Opaque keyword value
data KeywordValue where
  KeywordValue :: (Eq a, Show a, Typeable a) => a -> KeywordValue

instance Eq KeywordValue where
  (==) = error "KeywordValue equality: not yet implemented"

instance Show KeywordValue where
  show = error "KeywordValue show: not yet implemented"

-- | Dialect definition
data Dialect = Dialect
  { dialectURI :: Text
  , dialectName :: Text
  , dialectVersion :: Text
  , dialectVocabularies :: Map Text Bool  -- URI -> required?
  , dialectMetaSchema :: Schema
  , dialectDefaultFormat :: FormatBehavior
  , dialectUnknownKeywords :: UnknownKeywordMode
  } deriving (Eq, Show)

-- | Unknown keyword handling
data UnknownKeywordMode
  = IgnoreUnknown
  | WarnUnknown
  | ErrorOnUnknown
  | CollectUnknown
  deriving (Eq, Show)

-- | Format behavior
data FormatBehavior
  = FormatAssertion
  | FormatAnnotation
  deriving (Eq, Show)

-- | Vocabulary registry
data VocabularyRegistry = VocabularyRegistry
  { registeredVocabularies :: Map Text Vocabulary
  , registeredDialects :: Map Text Dialect
  , defaultDialect :: Maybe Dialect
  } deriving (Eq, Show)

-- | Empty registry
--
-- TODO: Implement with default vocabularies
emptyRegistry :: VocabularyRegistry
emptyRegistry = VocabularyRegistry Map.empty Map.empty Nothing

-- | Register a vocabulary
--
-- TODO: Implement vocabulary registration
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
registerVocabulary = error "registerVocabulary: not yet implemented"

-- | Register a dialect
--
-- TODO: Implement dialect registration
registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry
registerDialect = error "registerDialect: not yet implemented"

-- | Look up a vocabulary
--
-- TODO: Implement vocabulary lookup
lookupVocabulary :: Text -> VocabularyRegistry -> Maybe Vocabulary
lookupVocabulary = error "lookupVocabulary: not yet implemented"

-- | Look up a dialect
--
-- TODO: Implement dialect lookup
lookupDialect :: Text -> VocabularyRegistry -> Maybe Dialect
lookupDialect = error "lookupDialect: not yet implemented"

-- | Compose vocabularies into a dialect
--
-- TODO: Implement dialect composition
composeDialect
  :: Text                    -- Dialect name
  -> Text                    -- Dialect URI
  -> [(Text, Bool)]          -- Vocabularies (URI, required?)
  -> VocabularyRegistry
  -> Either Text Dialect
composeDialect = error "composeDialect: not yet implemented"

-- | Compose multiple vocabularies
--
-- TODO: Implement vocabulary composition
composeVocabularies :: [Vocabulary] -> Either Text Vocabulary
composeVocabularies = error "composeVocabularies: not yet implemented"

-- | Extend a vocabulary
--
-- TODO: Implement vocabulary extension
extendVocabulary
  :: Vocabulary                    -- Base vocabulary
  -> [(Text, KeywordDefinition)]  -- Additional keywords
  -> Vocabulary
extendVocabulary = error "extendVocabulary: not yet implemented"

-- | Create a vocabulary variant
--
-- TODO: Implement vocabulary variant creation
variantVocabulary
  :: Vocabulary                    -- Base
  -> (Vocabulary -> Vocabulary)    -- Modifications
  -> Vocabulary
variantVocabulary = error "variantVocabulary: not yet implemented"

-- Standard dialects (to be implemented)
draft04Dialect :: Dialect
draft04Dialect = error "draft04Dialect: not yet implemented"

draft06Dialect :: Dialect
draft06Dialect = error "draft06Dialect: not yet implemented"

draft07Dialect :: Dialect
draft07Dialect = error "draft07Dialect: not yet implemented"

draft201909Dialect :: Dialect
draft201909Dialect = error "draft201909Dialect: not yet implemented"

draft202012Dialect :: Dialect
draft202012Dialect = error "draft202012Dialect: not yet implemented"
