module Fractal.JsonSchema.Keyword.KeywordSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)

import Fractal.JsonSchema.Keyword
import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Compile
import Fractal.JsonSchema.Keyword.Validate
import Fractal.JsonSchema.Types (Schema, emptyPointer, ValidationResult(..), ValidationAnnotations)
import Fractal.JsonSchema.Parser (parseSchema)

-- | Test compiled data: minimum value for numbers
data MinValueData = MinValueData Double
  deriving (Show, Typeable)

-- | Compile function: extract minimum value from keyword value
compileMinValue :: CompileFunc MinValueData
compileMinValue (Number n) _schema _ctx = Right $ MinValueData (realToFrac n)
compileMinValue _ _schema _ctx = Left "x-min-value must be a number"

-- | Validate function: check instance is >= minimum
validateMinValue :: ValidateFunc MinValueData
validateMinValue _recursiveValidator (MinValueData minVal) _ctx (Number n) =
  if realToFrac n >= minVal
    then []
    else ["Value " <> T.pack (show n) <> " is less than minimum " <> T.pack (show minVal)]
validateMinValue _ _ _ _ = ["x-min-value can only validate numbers"]

-- | Test compiled data: string must match pattern
data PatternData = PatternData Text
  deriving (Show, Typeable)

compilePattern :: CompileFunc PatternData
compilePattern (String s) _schema _ctx = Right $ PatternData s
compilePattern _ _schema _ctx = Left "x-pattern must be a string"

validatePattern :: ValidateFunc PatternData
validatePattern _recursiveValidator (PatternData pat) _ctx (String s) =
  if pat `T.isInfixOf` s
    then []
    else ["String does not contain pattern: " <> pat]
validatePattern _ _ _ _ = ["x-pattern can only validate strings"]

spec :: Spec
spec = describe "Keyword System" $ do

  describe "Keyword Registration" $ do
    it "registers and looks up keywords" $ do
      let registry = registerKeyword
                       (mkKeywordDefinition "x-test" AnyScope compileMinValue validateMinValue)
                       emptyKeywordRegistry

      case lookupKeyword "x-test" registry of
        Nothing -> expectationFailure "Keyword not found after registration"
        Just def -> keywordName def `shouldBe` "x-test"

    it "returns Nothing for unregistered keywords" $ do
      case lookupKeyword "x-nonexistent" emptyKeywordRegistry of
        Nothing -> return ()
        Just _ -> expectationFailure "Should not have found unregistered keyword"

    it "can register multiple keywords" $ do
      let registry = registerKeyword
                       (mkKeywordDefinition "x-min" AnyScope compileMinValue validateMinValue)
                     $ registerKeyword
                       (mkKeywordDefinition "x-pattern" AnyScope compilePattern validatePattern)
                       emptyKeywordRegistry

      case lookupKeyword "x-min" registry of
        Nothing -> expectationFailure "x-min not found"
        Just _ -> return ()
      case lookupKeyword "x-pattern" registry of
        Nothing -> expectationFailure "x-pattern not found"
        Just _ -> return ()

    it "gets list of registered keyword names" $ do
      let registry = registerKeyword
                       (mkKeywordDefinition "x-first" AnyScope compileMinValue validateMinValue)
                     $ registerKeyword
                       (mkKeywordDefinition "x-second" AnyScope compilePattern validatePattern)
                       emptyKeywordRegistry

      let names = getRegisteredKeywords registry
      length names `shouldBe` 2
      "x-first" `elem` names `shouldBe` True
      "x-second" `elem` names `shouldBe` True

  describe "Keyword Compilation" $ do
    it "compiles a keyword with valid value" $ do
      let def = mkKeywordDefinition "x-min-value" AnyScope compileMinValue validateMinValue
          value = Number 10
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []

      case compileKeyword def value schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> compiledKeywordName compiled `shouldBe` "x-min-value"

    it "fails compilation with invalid value type" $ do
      let def = mkKeywordDefinition "x-min-value" AnyScope compileMinValue validateMinValue
          value = String "not a number"
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []

      case compileKeyword def value schema ctx of
        Left err -> err `shouldBe` "x-min-value must be a number"
        Right _ -> expectationFailure "Should have failed with wrong value type"

    it "compiles multiple keywords" $ do
      let minDef = mkKeywordDefinition "x-min" AnyScope compileMinValue validateMinValue
          patternDef = mkKeywordDefinition "x-pattern" AnyScope compilePattern validatePattern
          definitions = Map.fromList
            [ ("x-min", minDef)
            , ("x-pattern", patternDef)
            ]
          keywordValues = Map.fromList
            [ ("x-min", Number 5)
            , ("x-pattern", String "test")
            ]
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []

      case compileKeywords definitions keywordValues schema ctx of
        Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
        Right compiled -> do
          case lookupCompiledKeyword "x-min" compiled of
            Nothing -> expectationFailure "x-min not found in compiled keywords"
            Just _ -> return ()
          case lookupCompiledKeyword "x-pattern" compiled of
            Nothing -> expectationFailure "x-pattern not found in compiled keywords"
            Just _ -> return ()

  describe "Keyword Validation" $ do
    it "validates instance with compiled keyword" $ do
      let def = mkKeywordDefinition "x-min-value" AnyScope compileMinValue validateMinValue
          value = Number 10
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []
          compiled = either (error . T.unpack) id $ compileKeyword def value schema ctx
          compiledKeywords = addCompiledKeyword compiled emptyCompiledKeywords
          validationCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          -- Dummy recursive validator (not used in these simple tests)
          recursiveValidator = \_ _ -> ValidationSuccess mempty

      -- Valid: 15 >= 10
      validateKeywords recursiveValidator compiledKeywords (Number 15) validationCtx `shouldBe` []

      -- Invalid: 5 < 10
      let errors = validateKeywords recursiveValidator compiledKeywords (Number 5) validationCtx
      length errors `shouldBe` 1
      head errors `shouldSatisfy` T.isInfixOf "less than minimum"

    it "validates with multiple keywords" $ do
      let minDef = mkKeywordDefinition "x-min" AnyScope compileMinValue validateMinValue
          patternDef = mkKeywordDefinition "x-pattern" AnyScope compilePattern validatePattern
          definitions = Map.fromList
            [ ("x-min", minDef)
            , ("x-pattern", patternDef)
            ]
          keywordValues = Map.fromList
            [ ("x-min", Number 5)
            , ("x-pattern", String "hello")
            ]
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []
          compiled = either (error . T.unpack) id $ compileKeywords definitions keywordValues schema ctx
          validationCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          recursiveValidator = \_ _ -> ValidationSuccess mempty

      -- Test number validation
      let numErrors = validateKeywords recursiveValidator compiled (Number 10) validationCtx
      -- x-min succeeds (10 >= 5), x-pattern fails (can only validate strings)
      length numErrors `shouldBe` 1
      head numErrors `shouldSatisfy` T.isInfixOf "can only validate strings"

      -- Test string validation
      let strErrors = validateKeywords recursiveValidator compiled (String "hello world") validationCtx
      -- x-pattern succeeds (contains "hello"), x-min fails (can only validate numbers)
      length strErrors `shouldBe` 1
      head strErrors `shouldSatisfy` T.isInfixOf "can only validate numbers"

    it "collects all errors from all keywords" $ do
      let minDef = mkKeywordDefinition "x-min" AnyScope compileMinValue validateMinValue
          definitions = Map.fromList [("x-min", minDef)]
          keywordValues = Map.fromList [("x-min", Number 100)]
          schema = either (error . show) id $ parseSchema (Aeson.object [])
          ctx = buildCompilationContext Map.empty emptyKeywordRegistry schema []
          compiled = either (error . T.unpack) id $ compileKeywords definitions keywordValues schema ctx
          validationCtx = ValidationContext' { kwContextInstancePath = [], kwContextSchemaPath = [] }
          recursiveValidator = \_ _ -> ValidationSuccess mempty

      -- Value way below minimum
      let errors = validateKeywords recursiveValidator compiled (Number 1) validationCtx
      length errors `shouldBe` 1
      head errors `shouldSatisfy` T.isInfixOf "less than minimum"

  describe "Keyword Scope" $ do
    it "creates keywords with different scopes" $ do
      let appliesAnywhere = mkKeywordDefinition "x-anywhere" AnyScope compileMinValue validateMinValue
          appliesToStrings = mkKeywordDefinition "x-string" StringOnly compileMinValue validateMinValue

      keywordName appliesAnywhere `shouldBe` "x-anywhere"
      keywordName appliesToStrings `shouldBe` "x-string"

    it "shows keyword scope in Show instance" $ do
      show AnyScope `shouldBe` "AnyScope"
      show StringOnly `shouldBe` "StringOnly"
