-- | JSON Schema dialects
--
-- Dialect definitions for each supported JSON Schema version.
-- A dialect specifies which vocabularies are enabled and how keywords behave.
module Fractal.JsonSchema.Dialect
  ( Dialect(..)
  , FormatBehavior(..)
  , UnknownKeywordMode(..)
  , draft04Dialect
  , draft06Dialect
  , draft07Dialect
  , draft201909Dialect
  , draft202012Dialect
  , dialectURI
  ) where

import Fractal.JsonSchema.Types
import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Format keyword behavior
data FormatBehavior
  = FormatAssertion      -- ^ Format is validation constraint
  | FormatAnnotation     -- ^ Format is annotation only
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Unknown keyword handling strategy
data UnknownKeywordMode
  = IgnoreUnknown        -- ^ Silently ignore
  | WarnUnknown          -- ^ Emit warnings but continue
  | ErrorOnUnknown       -- ^ Fail parsing
  | CollectUnknown       -- ^ Collect in extensions map
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | A dialect is a complete configuration of vocabularies
data Dialect = Dialect
  { dialectVersion :: JsonSchemaVersion
  , dialectName :: Text
  , dialectVocabularies :: Map Text Bool  -- URI -> required?
  , dialectDefaultFormat :: FormatBehavior
  , dialectUnknownKeywords :: UnknownKeywordMode
  }
  deriving (Eq, Show)

-- | Get URI for a dialect
dialectURI :: Dialect -> Text
dialectURI d = case dialectVersion d of
  Draft04 -> "http://json-schema.org/draft-04/schema#"
  Draft06 -> "http://json-schema.org/draft-06/schema#"
  Draft07 -> "http://json-schema.org/draft-07/schema#"
  Draft201909 -> "https://json-schema.org/draft/2019-09/schema"
  Draft202012 -> "https://json-schema.org/draft/2020-12/schema"

-- | Draft-04 dialect
draft04Dialect :: Dialect
draft04Dialect = Dialect
  { dialectVersion = Draft04
  , dialectName = "JSON Schema Draft-04"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-04
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft-06 dialect (adds const, propertyNames)
draft06Dialect :: Dialect
draft06Dialect = Dialect
  { dialectVersion = Draft06
  , dialectName = "JSON Schema Draft-06"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-06
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft-07 dialect (adds if/then/else, readOnly/writeOnly, $comment)
draft07Dialect :: Dialect
draft07Dialect = Dialect
  { dialectVersion = Draft07
  , dialectName = "JSON Schema Draft-07"
  , dialectVocabularies = Map.empty  -- No $vocabulary keyword in draft-07
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft 2019-09 dialect (adds $vocabulary, unevaluated*, dependent*)
draft201909Dialect :: Dialect
draft201909Dialect = Dialect
  { dialectVersion = Draft201909
  , dialectName = "JSON Schema 2019-09"
  , dialectVocabularies = Map.fromList
      [ ("https://json-schema.org/draft/2019-09/vocab/core", True)
      , ("https://json-schema.org/draft/2019-09/vocab/applicator", True)
      , ("https://json-schema.org/draft/2019-09/vocab/validation", True)
      , ("https://json-schema.org/draft/2019-09/vocab/meta-data", False)
      , ("https://json-schema.org/draft/2019-09/vocab/format", False)
      , ("https://json-schema.org/draft/2019-09/vocab/content", False)
      ]
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

-- | Draft 2020-12 dialect (adds prefixItems, $dynamicRef)
draft202012Dialect :: Dialect
draft202012Dialect = Dialect
  { dialectVersion = Draft202012
  , dialectName = "JSON Schema 2020-12"
  , dialectVocabularies = Map.fromList
      [ ("https://json-schema.org/draft/2020-12/vocab/core", True)
      , ("https://json-schema.org/draft/2020-12/vocab/applicator", True)
      , ("https://json-schema.org/draft/2020-12/vocab/validation", True)
      , ("https://json-schema.org/draft/2020-12/vocab/unevaluated", False)
      , ("https://json-schema.org/draft/2020-12/vocab/meta-data", False)
      , ("https://json-schema.org/draft/2020-12/vocab/format-annotation", False)
      , ("https://json-schema.org/draft/2020-12/vocab/format-assertion", False)
      , ("https://json-schema.org/draft/2020-12/vocab/content", False)
      ]
  , dialectDefaultFormat = FormatAnnotation
  , dialectUnknownKeywords = CollectUnknown
  }

