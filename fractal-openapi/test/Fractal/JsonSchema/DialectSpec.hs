{-# LANGUAGE OverloadedStrings #-}

module Fractal.JsonSchema.DialectSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Fractal.JsonSchema.Dialect
import Fractal.JsonSchema.Types (JsonSchemaVersion(..))
import Fractal.JsonSchema.Vocabulary.Types
import Fractal.JsonSchema.Vocabulary.Registry
import Fractal.JsonSchema.Keyword.Types (KeywordDefinition(..), KeywordScope(..), KeywordNavigation(..))

spec :: Spec
spec = describe "Dialect" $ do
  
  describe "Predefined Dialects" $ do
    it "draft04Dialect has correct URI" $ do
      dialectURI draft04Dialect `shouldBe` "http://json-schema.org/draft-04/schema#"
    
    it "draft06Dialect has correct URI" $ do
      dialectURI draft06Dialect `shouldBe` "http://json-schema.org/draft-06/schema#"
    
    it "draft07Dialect has correct URI" $ do
      dialectURI draft07Dialect `shouldBe` "http://json-schema.org/draft-07/schema#"
    
    it "draft201909Dialect has correct URI" $ do
      dialectURI draft201909Dialect `shouldBe` "https://json-schema.org/draft/2019-09/schema"
    
    it "draft202012Dialect has correct URI" $ do
      dialectURI draft202012Dialect `shouldBe` "https://json-schema.org/draft/2020-12/schema"
    
    it "draft201909Dialect declares vocabularies" $ do
      dialectVocabularies draft201909Dialect `shouldSatisfy` (not . Map.null)
    
    it "draft202012Dialect declares vocabularies" $ do
      dialectVocabularies draft202012Dialect `shouldSatisfy` (not . Map.null)

  describe "validateDialectURI" $ do
    it "accepts http:// URIs" $ do
      validateDialectURI "http://example.com/schema" `shouldBe` Right ()
    
    it "accepts https:// URIs" $ do
      validateDialectURI "https://example.com/schema" `shouldBe` Right ()
    
    it "rejects empty URIs" $ do
      validateDialectURI "" `shouldSatisfy` isLeft
    
    it "rejects relative URIs" $ do
      validateDialectURI "/relative/path" `shouldSatisfy` isLeft
    
    it "rejects URIs without scheme" $ do
      validateDialectURI "example.com/schema" `shouldSatisfy` isLeft

  describe "validateDialect" $ do
    let emptyRegistry = emptyVocabularyRegistry
    
    it "accepts dialect with valid absolute URI" $ do
      let dialect = draft04Dialect
      validateDialect emptyRegistry dialect `shouldBe` Right ()
    
    it "rejects dialect with invalid URI" $ do
      let dialect = draft04Dialect { dialectURI = "not-absolute" }
      validateDialect emptyRegistry dialect `shouldSatisfy` isLeft
    
    it "rejects dialect with unresolvable vocabulary" $ do
      let dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Test Dialect"
            , dialectVocabularies = Map.singleton "https://example.com/unknown-vocab" True
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      case validateDialect emptyRegistry dialect of
        Left (UnresolvableVocabulary _) -> pure ()
        _ -> expectationFailure "Expected UnresolvableVocabulary error"
    
    it "detects keyword conflicts between vocabularies" $ do
      -- Create two vocabularies with conflicting keywords
      let keyword1 = KeywordDefinition
            { keywordName = "conflicting"
            , keywordScope = AnyScope
            , keywordCompile = \_ _ _ -> Left "Not implemented" :: Either Text ()
            , keywordValidate = \_ (_ :: ()) _ _ -> []
            , keywordNavigation = NoNavigation
            }
          vocab1 = Vocabulary
            { vocabularyURI = "https://example.com/vocab1"
            , vocabularyRequired = True
            , vocabularyKeywords = Map.singleton "conflicting" keyword1
            }
          vocab2 = Vocabulary
            { vocabularyURI = "https://example.com/vocab2"
            , vocabularyRequired = True
            , vocabularyKeywords = Map.singleton "conflicting" keyword1
            }
          registry = registerVocabulary vocab2 $ registerVocabulary vocab1 emptyRegistry
          dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Conflicting Dialect"
            , dialectVocabularies = Map.fromList
                [ ("https://example.com/vocab1", True)
                , ("https://example.com/vocab2", True)
                ]
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      
      case validateDialect registry dialect of
        Left (KeywordConflicts conflicts) -> do
          length conflicts `shouldBe` 1
          fst (head conflicts) `shouldBe` "conflicting"
        _ -> expectationFailure "Expected KeywordConflicts error"

  describe "FormatBehavior" $ do
    it "has FormatAssertion variant" $ do
      show FormatAssertion `shouldContain` "Assertion"
    
    it "has FormatAnnotation variant" $ do
      show FormatAnnotation `shouldContain` "Annotation"

  describe "UnknownKeywordMode" $ do
    it "has all variants" $ do
      let modes = [IgnoreUnknown, WarnUnknown, ErrorOnUnknown, CollectUnknown]
      length modes `shouldBe` 4

  describe "defaultDialectURI" $ do
    it "returns correct URI for Draft04" $ do
      defaultDialectURI Draft04 `shouldBe` "http://json-schema.org/draft-04/schema#"
    
    it "returns correct URI for Draft06" $ do
      defaultDialectURI Draft06 `shouldBe` "http://json-schema.org/draft-06/schema#"
    
    it "returns correct URI for Draft07" $ do
      defaultDialectURI Draft07 `shouldBe` "http://json-schema.org/draft-07/schema#"
    
    it "returns correct URI for Draft201909" $ do
      defaultDialectURI Draft201909 `shouldBe` "https://json-schema.org/draft/2019-09/schema"
    
    it "returns correct URI for Draft202012" $ do
      defaultDialectURI Draft202012 `shouldBe` "https://json-schema.org/draft/2020-12/schema"

  describe "Dialect composition" $ do
    it "composes dialect with valid vocabularies" $ do
      let vocab = Vocabulary
            { vocabularyURI = "https://example.com/vocab"
            , vocabularyRequired = False
            , vocabularyKeywords = Map.empty
            }
          registry = registerVocabulary vocab emptyVocabularyRegistry
          dialect = Dialect
            { dialectURI = "https://example.com/dialect"
            , dialectVersion = Draft202012
            , dialectName = "Example Dialect"
            , dialectVocabularies = Map.singleton "https://example.com/vocab" False
            , dialectDefaultFormat = FormatAnnotation
            , dialectUnknownKeywords = CollectUnknown
            }
      
      validateDialect registry dialect `shouldBe` Right ()
    
    it "preserves vocabulary requirements" $ do
      let dialect = draft201909Dialect
          vocabs = dialectVocabularies dialect
      
      -- Core vocabulary should be required
      Map.lookup "https://json-schema.org/draft/2019-09/vocab/core" vocabs `shouldBe` Just True
      
      -- Format vocabulary should be optional
      Map.lookup "https://json-schema.org/draft/2019-09/vocab/format" vocabs `shouldBe` Just False

-- Helper to check if Either is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
