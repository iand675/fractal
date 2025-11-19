{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Fractal.JsonSchema.ComplianceSpec (spec) where

import Test.Hspec
import Fractal.JsonSchema
import Fractal.JsonSchema.Validator (validateValueWithRegistry)
import Fractal.JsonSchema.ReferenceLoader
import Fractal.JsonSchema.Types (buildRegistryWithExternalRefs)
import Data.Aeson (Value, FromJSON(..), (.:), (.=), eitherDecodeFileStrict, object)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, makeRelative)
import Control.Monad (forM, forM_, when)

-- | Test suite file format
data TestSuiteFile = TestSuiteFile [TestGroup]
  deriving (Eq, Show, Generic)

instance FromJSON TestSuiteFile where
  parseJSON v = TestSuiteFile <$> parseJSON v

-- | A group of related tests
data TestGroup = TestGroup
  { groupDescription :: Text
  , groupSchema :: Value
  , groupTests :: [TestCase]
  }
  deriving (Eq, Show, Generic)

instance FromJSON TestGroup where
  parseJSON = Aeson.withObject "TestGroup" $ \o -> TestGroup
    <$> o .: "description"
    <*> o .: "schema"
    <*> o .: "tests"

-- | Individual test case
data TestCase = TestCase
  { testDescription :: Text
  , testData :: Value
  , testValid :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON TestCase where
  parseJSON = Aeson.withObject "TestCase" $ \o -> TestCase
    <$> o .: "description"
    <*> o .: "data"
    <*> o .: "valid"

-- | Reference loader for test suite remote schemas
-- Maps http://localhost:1234/... to local files in test-suite/json-schema-test-suite/remotes/
testSuiteLoader :: JsonSchemaVersion -> ReferenceLoader
testSuiteLoader version uri
  | T.isPrefixOf "http://localhost:1234/" uri = do
      let path = T.drop (T.length "http://localhost:1234/") uri
          -- Try version-specific directory first, then fall back to common remotes
          versionDir = case version of
            Draft04 -> "draft4"
            Draft06 -> "draft6"
            Draft07 -> "draft7"
            Draft201909 -> "draft2019-09"
            Draft202012 -> "draft2020-12"
          versionPath = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
          commonPath = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> T.unpack path
      
      -- Try version-specific path first
      versionResult <- fileLoader versionPath
      case versionResult of
        Right schema -> pure $ Right schema
        Left _ -> fileLoader commonPath  -- Fall back to common
  | normalizeMetaURI uri == Just "http://json-schema.org/draft-07/schema" =
      pure $ Right draft07MetaSchema
  | normalizeMetaURI uri == Just "https://json-schema.org/draft/2020-12/schema" =
      pure $ Right draft202012MetaSchema
  | normalizeMetaURI uri == Just "http://json-schema.org/draft/2019-09/schema" =
      pure $ Right draft201909MetaSchema
  -- Handle relative file paths (non-URI paths used in test suite)
  | not (T.isInfixOf "://" uri) && not (T.isPrefixOf "#" uri) = do
      -- It's a relative file path, try loading from remotes directory
      let versionDir = case version of
            Draft04 -> "draft4"
            Draft06 -> "draft6"
            Draft07 -> "draft7"
            Draft201909 -> "draft2019-09"
            Draft202012 -> "draft2020-12"
          versionPath = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack uri
          commonPath = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> T.unpack uri
      
      versionResult <- fileLoader versionPath
      case versionResult of
        Right schema -> pure $ Right schema
        Left _ -> fileLoader commonPath
  | otherwise = noOpLoader uri
  where
    normalizeMetaURI u =
      let base = T.takeWhile (/= '#') u
      in if T.null base then Nothing else Just base

-- | Minimal stub meta-schema to support remote ref tests
draft07MetaSchema :: Schema
draft07MetaSchema =
  case parseSchema metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-07 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft-07/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft201909MetaSchema :: Schema
draft201909MetaSchema =
  case parseSchemaWithVersion Draft201909 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft 2019-09 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft/2019-09/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "additionalProperties" .= True
        ]

draft202012MetaSchema :: Schema
draft202012MetaSchema =
  case parseSchemaWithVersion Draft202012 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft 2020-12 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("https://json-schema.org/draft/2020-12/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "additionalProperties" .= True
        ]

-- | Run a single test case
runTestCase :: JsonSchemaVersion -> TestGroup -> TestCase -> Expectation
runTestCase version group testCase = do
  -- Parse the schema
  case parseSchemaWithVersion version (groupSchema group) of
    Left parseErr -> expectationFailure $ 
      "Schema parse failed: " <> T.unpack (groupDescription group) 
      <> " - " <> show parseErr
    Right schema -> do
      -- Validate the test data (without external refs for now)
      -- TODO: Enable external ref loading once base URI tracking is complete
      let loader = testSuiteLoader version
      registryResult <- buildRegistryWithExternalRefs loader schema
      case registryResult of
        Left err ->
          expectationFailure $ T.unpack $
            T.unlines
              [ "Failed to build schema registry for " <> groupDescription group
              , "Error: " <> err
              ]
        Right registry -> do
          let config = defaultValidationConfig { validationVersion = version }
              result = validateValueWithRegistry config registry schema (testData testCase)
              actualValid = isSuccess result
              expectedValid = testValid testCase
          when (actualValid /= expectedValid) $ do
            expectationFailure $ T.unpack $ T.unlines
              [ "Test failed: " <> testDescription testCase
              , "Group: " <> groupDescription group
              , "Expected: " <> if expectedValid then "valid" else "invalid"
              , "Actual: " <> if actualValid then "valid" else "invalid"
              , case result of
                  ValidationFailure errs -> "Errors: " <> T.pack (show $ unErrors errs)
                  ValidationSuccess _ -> "Validated successfully"
              ]

-- | Load and run tests from a file, creating individual it blocks
runTestFile :: JsonSchemaVersion -> FilePath -> Spec
runTestFile version filePath = describe fileLabel $ do
  result <- runIO $ eitherDecodeFileStrict filePath
  case result of
    Left err -> it ("Failed to load " <> fileLabel) $
      expectationFailure $ "JSON parse error: " <> err
    Right (TestSuiteFile groups) ->
      forM_ groups $ \group ->
        describe (T.unpack $ groupDescription group) $
          forM_ (groupTests group) $ \testCase ->
            it (T.unpack $ testDescription testCase) $
              runTestCase version group testCase
  where
    suiteRoot = "test-suite/json-schema-test-suite/tests"
    fileLabel = makeRelative suiteRoot filePath

collectJsonFiles :: FilePath -> IO [FilePath]
collectJsonFiles dir = do
  entries <- listDirectory dir
  fmap concat $
    forM (sort entries) $ \entry -> do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then collectJsonFiles path
        else pure [path | takeExtension path == ".json"]

spec :: Spec
spec = do
  describe "JSON Schema Official Test Suite" $ do
    it "loads test suite structure" $ do
      -- Verify test suite is available
      let testSuiteRoot = "test-suite/json-schema-test-suite/tests"
      exists <- doesFileExist (testSuiteRoot </> "draft7/type.json")
      exists `shouldBe` True
    
    let testSuiteRoot = "test-suite/json-schema-test-suite/tests"
        versionDirs =
          [ (Draft04, "draft4")
          , (Draft06, "draft6")
          , (Draft07, "draft7")
          , (Draft201909, "draft2019-09")
          , (Draft202012, "draft2020-12")
          ]
    
    forM_ versionDirs $ \(version, dirName) -> do
      describe ("Draft " <> show version) $ do
        files <- runIO $ collectJsonFiles (testSuiteRoot </> dirName)
        forM_ files $ \file ->
          runTestFile version file
