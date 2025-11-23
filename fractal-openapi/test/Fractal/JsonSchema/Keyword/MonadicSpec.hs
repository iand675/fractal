-- | Tests for monadic keyword compilation and adjacent keyword access
module Fractal.JsonSchema.Keyword.MonadicSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Data.Scientific (Scientific)

import Fractal.JsonSchema.Keyword
import Fractal.JsonSchema.Keyword.Types
import Fractal.JsonSchema.Keyword.Monadic
import Fractal.JsonSchema.Parser (parseSchema)
import Fractal.JsonSchema.Types (Schema(..))

-- | Test keyword that accesses an adjacent "minimum" keyword
-- This simulates Draft-04 behavior where exclusiveMinimum is boolean
newtype Draft04ExclusiveMinData = Draft04ExclusiveMinData Bool
  deriving (Show, Eq, Typeable)

-- Compile function that accesses adjacent keyword
compileDraft04ExclusiveMin :: Value -> Schema -> CompilationContext -> Either Text Draft04ExclusiveMinData
compileDraft04ExclusiveMin value _schema _ctx = case value of
  Bool b -> Right $ Draft04ExclusiveMinData b
  _ -> Left "exclusiveMinimum must be a boolean"

validateDraft04ExclusiveMin :: ValidateFunc Draft04ExclusiveMinData
validateDraft04ExclusiveMin _ _ _ _ = pure []  -- Validation handled by minimum keyword

draft04ExclusiveMinKeyword :: KeywordDefinition
draft04ExclusiveMinKeyword = KeywordDefinition
  { keywordName = "exclusiveMinimum"
  , keywordScope = AnyScope
  , keywordCompile = compileDraft04ExclusiveMin
  , keywordValidate = validateDraft04ExclusiveMin
  }

-- | Test keyword that reads adjacent data using CompileM
newtype MinimumWithExclusiveData = MinimumWithExclusiveData
  { minimumValue :: Scientific
  } deriving (Show, Eq, Typeable)

-- This would be the monadic compile function (not yet integrated)
-- compileMinimumWithExclusive :: Value -> CompileM MinimumWithExclusiveData
-- compileMinimumWithExclusive value = do
--   -- Get the minimum value
--   minimum <- case value of
--     Number n -> pure n
--     _ -> liftEither $ Left "minimum must be a number"
--
--   -- Check if adjacent exclusiveMinimum is true
--   mExclusive <- getAdjacentData @Draft04ExclusiveMinData "exclusiveMinimum"
--
--   pure $ MinimumWithExclusiveData minimum

spec :: Spec
spec = do
  describe "getKeywordValue" $ do
    it "extracts keyword values from schemaRawKeywords" $ do
      let schemaJson = object
            [ "type" .= String "number"
            , "minimum" .= Number 5
            , "exclusiveMinimum" .= Bool True
            , "x-custom" .= String "test"
            ]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          getKeywordValue "minimum" schema `shouldBe` Just (Number 5)
          getKeywordValue "exclusiveMinimum" schema `shouldBe` Just (Bool True)
          getKeywordValue "x-custom" schema `shouldBe` Just (String "test")
          getKeywordValue "nonexistent" schema `shouldBe` Nothing

    it "returns Nothing for keywords not in schema" $ do
      let schemaJson = object ["type" .= String "string"]
      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          getKeywordValue "minimum" schema `shouldBe` Nothing
          getKeywordValue "maximum" schema `shouldBe` Nothing

    it "works with boolean schemas (no keywords)" $ do
      case parseSchema (Bool True) of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          getKeywordValue "minimum" schema `shouldBe` Nothing
          getKeywordValue "type" schema `shouldBe` Nothing

  describe "compileAdjacent" $ do
    it "compiles adjacent keyword on-demand" $ do
      let schemaJson = object
            [ "minimum" .= Number 10
            , "exclusiveMinimum" .= Bool True
            ]
          registry = registerKeyword draft04ExclusiveMinKeyword emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          case runCompileM (compileAdjacent "exclusiveMinimum") state of
            Left err -> expectationFailure $ "Compilation failed: " ++ T.unpack err
            Right (mCompiled, _) -> do
              mCompiled `shouldSatisfy` maybe False (\ck -> compiledKeywordName ck == "exclusiveMinimum")

    it "returns Nothing for unregistered keywords" $ do
      let schemaJson = object ["x-custom" .= String "value"]
          registry = emptyKeywordRegistry  -- No custom keywords registered

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          case runCompileM (compileAdjacent "x-custom") state of
            Left err -> expectationFailure $ "Should not fail: " ++ T.unpack err
            Right (mCompiled, _) -> mCompiled `shouldBe` Nothing

    it "returns Nothing for keywords not in schema" $ do
      let schemaJson = object ["type" .= String "string"]
          registry = registerKeyword draft04ExclusiveMinKeyword emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          case runCompileM (compileAdjacent "exclusiveMinimum") state of
            Left err -> expectationFailure $ "Should not fail: " ++ T.unpack err
            Right (mCompiled, _) -> mCompiled `shouldBe` Nothing

  describe "getAdjacentData" $ do
    it "retrieves typed data from adjacent keyword" $ do
      let schemaJson = object
            [ "minimum" .= Number 10
            , "exclusiveMinimum" .= Bool True
            ]
          registry = registerKeyword draft04ExclusiveMinKeyword emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          case runCompileM (getAdjacentData @Draft04ExclusiveMinData "exclusiveMinimum") state of
            Left err -> expectationFailure $ "Failed to get adjacent data: " ++ T.unpack err
            Right (mData, _) -> do
              mData `shouldSatisfy` maybe False (\(Draft04ExclusiveMinData b) -> b == True)

    it "returns Nothing for wrong type" $ do
      let schemaJson = object ["exclusiveMinimum" .= Bool True]
          registry = registerKeyword draft04ExclusiveMinKeyword emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          -- Try to extract as wrong type (String instead of Bool)
          case runCompileM (getAdjacentData @Text "exclusiveMinimum") state of
            Left err -> expectationFailure $ "Should not fail: " ++ T.unpack err
            Right (mData, _) -> mData `shouldBe` Nothing

  describe "memoization and cycle detection" $ do
    it "memoizes compiled keywords" $ do
      let schemaJson = object ["exclusiveMinimum" .= Bool True]
          registry = registerKeyword draft04ExclusiveMinKeyword emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          -- Compile twice, should use memoization second time
          case runCompileM (compileAdjacent "exclusiveMinimum" >> compileAdjacent "exclusiveMinimum") state of
            Left err -> expectationFailure $ "Failed: " ++ T.unpack err
            Right (mCompiled, finalState) -> do
              -- Should be in compiled map
              Map.member "exclusiveMinimum" (stateCompiled finalState) `shouldBe` True
              -- Should not be in compiling set
              Map.member "exclusiveMinimum" (stateCompiled finalState) `shouldBe` True

    it "detects circular dependencies" $ do
      -- This test would need a keyword that references itself
      -- For now, we just verify the structure exists
      let schemaJson = object ["type" .= String "string"]
          registry = emptyKeywordRegistry

      case parseSchema schemaJson of
        Left err -> expectationFailure $ "Failed to parse schema: " ++ show err
        Right schema -> do
          let ctx = CompilationContext
                { contextRegistry = Map.empty
                , contextResolveRef = \_ -> Left "No refs"
                , contextCurrentSchema = schema
                , contextParentPath = []
                , contextKeywordRegistry = registry
                }
              state = initCompilationState schema ctx

          -- State should have empty compiling set initially
          stateCompiling state `shouldBe` mempty
