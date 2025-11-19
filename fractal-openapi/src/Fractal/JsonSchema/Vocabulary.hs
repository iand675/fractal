-- | Extensible vocabulary system
--
-- Define and register custom keywords for domain-specific validation.
-- Implements the vocabulary system for JSON Schema 2019-09 and 2020-12.
module Fractal.JsonSchema.Vocabulary
  ( -- * Vocabulary Types
    Vocabulary(..)
  , KeywordDefinition(..)
  , KeywordValue(..)
  , KeywordScope(..)
  
    -- * Registry
  , VocabularyRegistry(..)
  , emptyVocabularyRegistry
  , standardRegistry
  , registerVocabulary
  , registerDialect
  , lookupVocabulary
  , lookupDialect
  , composeDialect
  , extractKeywordValue
  , detectKeywordConflicts
  
    -- * Standard Vocabularies
  , coreVocabulary
  , applicatorVocabulary
  , validationVocabulary
  , metadataVocabulary
  , formatAnnotationVocabulary
  , unevaluatedVocabulary
  ) where

import Fractal.JsonSchema.Types
import Fractal.JsonSchema.Dialect
import Data.Text (Text)
import qualified Data.Text
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable, eqT, (:~:)(Refl))

-- | Which schema types a keyword can appear in
data KeywordScope
  = AnySchema
  | ObjectSchemaOnly
  | ArraySchemaOnly
  | StringSchemaOnly
  | NumericSchemaOnly
  | BooleanSchemaOnly
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Opaque keyword value with type safety via existential
data KeywordValue where
  KeywordValue :: (Eq a, Show a, Typeable a) => a -> KeywordValue

instance Eq KeywordValue where
  (KeywordValue (a :: a)) == (KeywordValue (b :: b)) =
    case eqT @a @b of
      Just Refl -> a == b
      Nothing -> False

instance Show KeywordValue where
  show (KeywordValue a) = show a

-- | Keyword definition
data KeywordDefinition = KeywordDefinition
  { keywordName :: Text
  , keywordScope :: KeywordScope
  , keywordPriority :: Int  -- Higher = evaluated first
  }
  deriving (Eq, Show)

-- | Vocabulary definition
data Vocabulary = Vocabulary
  { vocabularyURI :: Text
  , vocabularyRequired :: Bool
  , vocabularyKeywords :: Map Text KeywordDefinition
  }
  deriving (Eq, Show)

-- | Vocabulary registry
data VocabularyRegistry = VocabularyRegistry
  { registeredVocabularies :: Map Text Vocabulary
  , registeredDialects :: Map Text Dialect
  }
  deriving (Eq, Show)

-- | Empty registry
emptyVocabularyRegistry :: VocabularyRegistry
emptyVocabularyRegistry = VocabularyRegistry Map.empty Map.empty

-- | Standard registry with all standard vocabularies and dialects
standardRegistry :: VocabularyRegistry
standardRegistry = foldr registerDialect
  (foldr registerVocabulary emptyVocabularyRegistry standardVocabularies)
  standardDialects
  where
    standardVocabularies =
      [ coreVocabulary
      , applicatorVocabulary
      , validationVocabulary
      , metadataVocabulary
      , formatAnnotationVocabulary
      , unevaluatedVocabulary
      ]
    standardDialects =
      [ draft04Dialect
      , draft06Dialect
      , draft07Dialect
      , draft201909Dialect
      , draft202012Dialect
      ]

-- | Register a vocabulary
registerVocabulary :: Vocabulary -> VocabularyRegistry -> VocabularyRegistry
registerVocabulary vocab reg = reg
  { registeredVocabularies = Map.insert (vocabularyURI vocab) vocab (registeredVocabularies reg)
  }

-- | Register a dialect
registerDialect :: Dialect -> VocabularyRegistry -> VocabularyRegistry
registerDialect dialect reg = reg
  { registeredDialects = Map.insert (dialectURI dialect) dialect (registeredDialects reg)
  }

-- | Lookup vocabulary by URI
lookupVocabulary :: Text -> VocabularyRegistry -> Maybe Vocabulary
lookupVocabulary uri reg = Map.lookup uri (registeredVocabularies reg)

-- | Lookup dialect by URI
lookupDialect :: Text -> VocabularyRegistry -> Maybe Dialect
lookupDialect uri reg = Map.lookup uri (registeredDialects reg)

-- | Compose a dialect from a list of vocabulary URIs
-- Returns Left with error message if:
-- - A required vocabulary is not found in the registry
-- - There are keyword conflicts between vocabularies
composeDialect 
  :: Text                      -- ^ Dialect name
  -> JsonSchemaVersion         -- ^ Schema version
  -> [(Text, Bool)]            -- ^ (vocabulary URI, required?)
  -> FormatBehavior            -- ^ Default format behavior
  -> UnknownKeywordMode        -- ^ Unknown keyword mode
  -> VocabularyRegistry        -- ^ Registry to lookup vocabularies
  -> Either Text Dialect       -- ^ Composed dialect or error
composeDialect name version vocabSpecs formatBehavior unknownMode reg = do
  -- Lookup all vocabularies
  vocabs <- mapM lookupVocabWithCheck vocabSpecs
  
  -- Detect keyword conflicts
  case detectKeywordConflicts vocabs of
    [] -> do
      -- No conflicts, create dialect
      let vocabMap = Map.fromList vocabSpecs
      Right $ Dialect
        { dialectVersion = version
        , dialectName = name
        , dialectVocabularies = vocabMap
        , dialectDefaultFormat = formatBehavior
        , dialectUnknownKeywords = unknownMode
        }
    conflicts -> Left $ "Keyword conflicts detected: " <> Data.Text.intercalate ", " conflicts
  where
    lookupVocabWithCheck (vocabURI, required) =
      case lookupVocabulary vocabURI reg of
        Just vocab -> Right vocab
        Nothing -> 
          if required
            then Left $ "Required vocabulary not found: " <> vocabURI
            else Left $ "Vocabulary not found: " <> vocabURI

-- | Detect keyword conflicts between vocabularies
-- Returns list of conflicting keyword names
detectKeywordConflicts :: [Vocabulary] -> [Text]
detectKeywordConflicts vocabs =
  let allKeywords = concatMap (\v -> Map.keys (vocabularyKeywords v)) vocabs
      keywordCounts = Map.fromListWith (+) [(k, 1 :: Int) | k <- allKeywords]
      conflicts = Map.keys $ Map.filter (> 1) keywordCounts
  in conflicts

-- | Extract a typed value from KeywordValue
-- Returns Nothing if the type doesn't match
extractKeywordValue :: forall a. Typeable a => KeywordValue -> Maybe a
extractKeywordValue (KeywordValue (val :: b)) =
  case eqT @a @b of
    Just Refl -> Just val
    Nothing -> Nothing

-- | Core vocabulary (2020-12)
coreVocabulary :: Vocabulary
coreVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/core"
  , vocabularyRequired = True
  , vocabularyKeywords = Map.fromList
      [ ("$schema", KeywordDefinition "$schema" AnySchema 200)
      , ("$id", KeywordDefinition "$id" AnySchema 200)
      , ("$ref", KeywordDefinition "$ref" AnySchema 150)
      , ("$anchor", KeywordDefinition "$anchor" AnySchema 200)
      , ("$dynamicRef", KeywordDefinition "$dynamicRef" AnySchema 150)
      , ("$dynamicAnchor", KeywordDefinition "$dynamicAnchor" AnySchema 200)
      , ("$vocabulary", KeywordDefinition "$vocabulary" AnySchema 200)
      , ("$comment", KeywordDefinition "$comment" AnySchema 10)
      , ("$defs", KeywordDefinition "$defs" AnySchema 200)
      ]
  }

-- | Applicator vocabulary (2020-12)
applicatorVocabulary :: Vocabulary
applicatorVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/applicator"
  , vocabularyRequired = True
  , vocabularyKeywords = Map.fromList
      [ ("prefixItems", KeywordDefinition "prefixItems" ArraySchemaOnly 100)
      , ("items", KeywordDefinition "items" ArraySchemaOnly 100)
      , ("contains", KeywordDefinition "contains" ArraySchemaOnly 100)
      , ("additionalProperties", KeywordDefinition "additionalProperties" ObjectSchemaOnly 100)
      , ("properties", KeywordDefinition "properties" ObjectSchemaOnly 100)
      , ("patternProperties", KeywordDefinition "patternProperties" ObjectSchemaOnly 100)
      , ("dependentSchemas", KeywordDefinition "dependentSchemas" ObjectSchemaOnly 100)
      , ("propertyNames", KeywordDefinition "propertyNames" ObjectSchemaOnly 100)
      , ("if", KeywordDefinition "if" AnySchema 90)
      , ("then", KeywordDefinition "then" AnySchema 90)
      , ("else", KeywordDefinition "else" AnySchema 90)
      , ("allOf", KeywordDefinition "allOf" AnySchema 90)
      , ("anyOf", KeywordDefinition "anyOf" AnySchema 90)
      , ("oneOf", KeywordDefinition "oneOf" AnySchema 90)
      , ("not", KeywordDefinition "not" AnySchema 90)
      ]
  }

-- | Validation vocabulary (2020-12)
validationVocabulary :: Vocabulary
validationVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/validation"
  , vocabularyRequired = True
  , vocabularyKeywords = Map.fromList
      [ ("type", KeywordDefinition "type" AnySchema 110)
      , ("enum", KeywordDefinition "enum" AnySchema 110)
      , ("const", KeywordDefinition "const" AnySchema 110)
      , ("multipleOf", KeywordDefinition "multipleOf" NumericSchemaOnly 100)
      , ("maximum", KeywordDefinition "maximum" NumericSchemaOnly 100)
      , ("exclusiveMaximum", KeywordDefinition "exclusiveMaximum" NumericSchemaOnly 100)
      , ("minimum", KeywordDefinition "minimum" NumericSchemaOnly 100)
      , ("exclusiveMinimum", KeywordDefinition "exclusiveMinimum" NumericSchemaOnly 100)
      , ("maxLength", KeywordDefinition "maxLength" StringSchemaOnly 100)
      , ("minLength", KeywordDefinition "minLength" StringSchemaOnly 100)
      , ("pattern", KeywordDefinition "pattern" StringSchemaOnly 100)
      , ("maxItems", KeywordDefinition "maxItems" ArraySchemaOnly 100)
      , ("minItems", KeywordDefinition "minItems" ArraySchemaOnly 100)
      , ("uniqueItems", KeywordDefinition "uniqueItems" ArraySchemaOnly 100)
      , ("maxContains", KeywordDefinition "maxContains" ArraySchemaOnly 100)
      , ("minContains", KeywordDefinition "minContains" ArraySchemaOnly 100)
      , ("maxProperties", KeywordDefinition "maxProperties" ObjectSchemaOnly 100)
      , ("minProperties", KeywordDefinition "minProperties" ObjectSchemaOnly 100)
      , ("required", KeywordDefinition "required" ObjectSchemaOnly 100)
      , ("dependentRequired", KeywordDefinition "dependentRequired" ObjectSchemaOnly 100)
      ]
  }

-- | Metadata vocabulary (2020-12)
metadataVocabulary :: Vocabulary
metadataVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/meta-data"
  , vocabularyRequired = False
  , vocabularyKeywords = Map.fromList
      [ ("title", KeywordDefinition "title" AnySchema 10)
      , ("description", KeywordDefinition "description" AnySchema 10)
      , ("default", KeywordDefinition "default" AnySchema 10)
      , ("deprecated", KeywordDefinition "deprecated" AnySchema 10)
      , ("readOnly", KeywordDefinition "readOnly" AnySchema 10)
      , ("writeOnly", KeywordDefinition "writeOnly" AnySchema 10)
      , ("examples", KeywordDefinition "examples" AnySchema 10)
      ]
  }

-- | Format annotation vocabulary (2020-12)
formatAnnotationVocabulary :: Vocabulary
formatAnnotationVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/format-annotation"
  , vocabularyRequired = False
  , vocabularyKeywords = Map.fromList
      [ ("format", KeywordDefinition "format" StringSchemaOnly 100)
      ]
  }

-- | Unevaluated vocabulary (2020-12)
unevaluatedVocabulary :: Vocabulary
unevaluatedVocabulary = Vocabulary
  { vocabularyURI = "https://json-schema.org/draft/2020-12/vocab/unevaluated"
  , vocabularyRequired = False
  , vocabularyKeywords = Map.fromList
      [ ("unevaluatedItems", KeywordDefinition "unevaluatedItems" ArraySchemaOnly 50)
      , ("unevaluatedProperties", KeywordDefinition "unevaluatedProperties" ObjectSchemaOnly 50)
      ]
  }

