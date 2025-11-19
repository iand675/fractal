{-# LANGUAGE OverloadedStrings #-}
module Fractal.OpenApi.JsonSchema.PatternSpec (spec) where

import Test.Hspec
import Fractal.OpenApi.JsonSchema.Validator
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (forM_)

-- | Test case from JSON Schema Test Suite
data TestCase = TestCase
  { tcDescription :: T.Text
  , tcData :: Value
  , tcValid :: Bool
  } deriving (Show)

-- | Test group from JSON Schema Test Suite
data TestGroup = TestGroup
  { tgDescription :: T.Text
  , tgPattern :: T.Text
  , tgTests :: [TestCase]
  } deriving (Show)

instance FromJSON TestCase where
  parseJSON = withObject "TestCase" $ \o -> TestCase
    <$> o .: "description"
    <*> o .: "data"
    <*> o .: "valid"

instance FromJSON TestGroup where
  parseJSON = withObject "TestGroup" $ \o -> do
    desc <- o .: "description"
    schema <- o .: "schema"
    -- Try to get pattern, return Nothing if it doesn't exist
    maybePattern <- schema .:? "pattern"
    tests <- o .: "tests"
    case maybePattern of
      Just pattern -> return $ TestGroup desc pattern tests
      Nothing -> fail "No pattern field in schema"

-- | Load and parse a test suite file, filtering out test groups without pattern
loadTestSuite :: FilePath -> IO (Either String [TestGroup])
loadTestSuite path = do
  content <- BL.readFile path
  case eitherDecode content of
    Left err -> return $ Left err
    Right groups -> return $ Right $ filterPatternTests groups
  where
    filterPatternTests :: [Value] -> [TestGroup]
    filterPatternTests = foldr extractPattern []

    extractPattern :: Value -> [TestGroup] -> [TestGroup]
    extractPattern val acc = case fromJSON val of
      Success tg -> tg : acc
      Error _ -> acc  -- Skip test groups without pattern field

-- | Run a single test case
runTestCase :: T.Text -> TestCase -> Expectation
runTestCase pattern tc = case tcData tc of
  String str -> do
    let result = validatePattern pattern str
    case result of
      Left err -> expectationFailure $ T.unpack $ "Pattern compilation failed: " <> err
      Right matched ->
        if tcValid tc
          then matched `shouldBe` True
          else matched `shouldBe` False
  -- Non-string types should always be valid (pattern is ignored)
  _ -> tcValid tc `shouldBe` True

-- | Run a test group
runTestGroup :: TestGroup -> Spec
runTestGroup tg =
  describe (T.unpack $ tgDescription tg) $ do
    forM_ (tgTests tg) $ \tc ->
      it (T.unpack $ tcDescription tc) $
        runTestCase (tgPattern tg) tc

spec :: Spec
spec = do
  describe "JSON Schema Test Suite - Pattern Tests" $ do
    describe "Draft 7 - Mandatory pattern tests" $ do
      let testFile = "/tmp/JSON-Schema-Test-Suite/tests/draft7/pattern.json"
      let testGroups = unsafePerformIO $ loadTestSuite testFile
      case testGroups of
        Left err -> it "loads test suite" $ expectationFailure $ "Failed to load test suite: " <> err
        Right groups -> forM_ groups runTestGroup

    describe "Draft 7 - ECMA-262 regex tests (optional)" $ do
      let testFile = "/tmp/JSON-Schema-Test-Suite/tests/draft7/optional/ecmascript-regex.json"
      let testGroups = unsafePerformIO $ loadTestSuite testFile
      case testGroups of
        Left err -> it "loads test suite" $ expectationFailure $ "Failed to load test suite: " <> err
        Right groups -> forM_ groups runTestGroup

    describe "Draft 2020-12 - Mandatory pattern tests" $ do
      let testFile = "/tmp/JSON-Schema-Test-Suite/tests/draft2020-12/pattern.json"
      let testGroups = unsafePerformIO $ loadTestSuite testFile
      case testGroups of
        Left err -> it "loads test suite" $ expectationFailure $ "Failed to load test suite: " <> err
        Right groups -> forM_ groups runTestGroup

    describe "Draft 2020-12 - ECMA-262 regex tests (optional)" $ do
      let testFile = "/tmp/JSON-Schema-Test-Suite/tests/draft2020-12/optional/ecmascript-regex.json"
      let testGroups = unsafePerformIO $ loadTestSuite testFile
      case testGroups of
        Left err -> it "loads test suite" $ expectationFailure $ "Failed to load test suite: " <> err
        Right groups -> forM_ groups runTestGroup
