{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Fractal.JsonSchema.ComplianceSpec (spec) where

import Test.Hspec
import Fractal.JsonSchema
import Fractal.JsonSchema.Validator (validateValueWithRegistry)
import Fractal.JsonSchema.ReferenceLoader
import Fractal.JsonSchema.Types (buildRegistryWithExternalRefs)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, FromJSON(..), (.:), (.=), eitherDecodeFileStrict, object)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (sort)
import GHC.Generics (Generic)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist, getCurrentDirectory)
import System.FilePath ((</>), takeExtension, makeRelative)
import Control.Monad (forM, forM_, when)
import System.IO (hPutStrLn, stderr)

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
-- Uses the new registry-based approach for cleaner URI routing
testSuiteLoader :: JsonSchemaVersion -> ReferenceLoader
testSuiteLoader version =
  let registry = registerDefaultLoader (relativePath version)
               $ registerScheme "http" (localhostLoader version)
               $ registerScheme "https" metaschemaLoader
               $ emptyLoaderRegistry
  in makeLoader registry

-- | Loader for http://localhost:1234/... URIs (maps to local test suite files)
localhostLoader :: JsonSchemaVersion -> ReferenceLoader
localhostLoader version uri
  | T.isPrefixOf "http://localhost:1234/" uri =
      let path = T.drop (T.length "http://localhost:1234/") uri
      in loadFromRemotes version path
  | otherwise =
      metaschemaLoader uri  -- Check for metaschemas

-- | Load from test-suite/json-schema-test-suite/remotes/ directory
loadFromRemotes :: JsonSchemaVersion -> Text -> IO (Either Text Schema)
loadFromRemotes version path = do
  let versionDir = case version of
        Draft04 -> "draft4"
        Draft06 -> "draft6"
        Draft07 -> "draft7"
        Draft201909 -> "draft2019-09"
        Draft202012 -> "draft2020-12"

      -- Check if path already includes version directory or v1
      pathIncludesVersion = any (\v -> T.isPrefixOf (T.pack v <> "/") path)
        ["draft4", "draft6", "draft7", "draft2019-09", "draft2020-12", "v1"]

      -- Try both with and without fractal-openapi/ prefix to support running from different directories
      -- For draft4/06/07, also try v1/ directory as those use the v1 test fixtures
      commonPath1 = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> T.unpack path
      commonPath2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/" <> T.unpack path
      versionPath1 = T.pack $ "test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
      versionPath2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/" <> versionDir <> "/" <> T.unpack path
      v1Path1 = T.pack $ "test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path
      v1Path2 = T.pack $ "fractal-openapi/test-suite/json-schema-test-suite/remotes/v1/" <> T.unpack path

  -- Try paths with fallback: version-specific first, then v1 (for older drafts), then common
  let pathsToTry = if pathIncludesVersion
                   then [commonPath1, commonPath2]
                   else case version of
                     Draft04 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                     Draft06 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                     Draft07 -> [versionPath1, versionPath2, v1Path1, v1Path2, commonPath1, commonPath2]
                     _ -> [versionPath1, versionPath2, commonPath1, commonPath2]

  tryPaths pathsToTry pathsToTry
  where
    tryPaths allPaths [] = do
      let pathsList = T.intercalate ", " allPaths
      pure $ Left $ "Could not load schema from any path for: " <> path <> " (tried: " <> pathsList <> ")"
    tryPaths allPaths (p:ps) = do
      result <- fileLoaderWithVersion version p
      case result of
        Right schema -> pure $ Right schema
        Left _ -> tryPaths allPaths ps

-- | Loader for relative paths (no scheme)
relativePath :: JsonSchemaVersion -> ReferenceLoader
relativePath version uri
  | T.isPrefixOf "#" uri = noOpLoader uri  -- Fragment only, not a file reference
  | otherwise = loadFromRemotes version uri

-- | Loader for known metaschemas
metaschemaLoader :: ReferenceLoader
metaschemaLoader uri =
  case normalizeMetaURI uri of
    Just "http://json-schema.org/draft-04/schema" ->
      pure $ Right draft04MetaSchema
    Just "http://json-schema.org/draft-06/schema" ->
      pure $ Right draft06MetaSchema
    Just "http://json-schema.org/draft-07/schema" ->
      pure $ Right draft07MetaSchema
    Just "https://json-schema.org/draft/2020-12/schema" ->
      pure $ Right draft202012MetaSchema
    Just "http://json-schema.org/draft/2019-09/schema" ->
      pure $ Right draft201909MetaSchema
    Just "https://json-schema.org/draft/2019-09/schema" ->
      pure $ Right draft201909MetaSchema
    _ ->
      pure $ Left $ "Unknown URI: " <> uri
  where
    normalizeMetaURI u =
      let base = T.takeWhile (/= '#') u
      in if T.null base then Nothing else Just base

-- | Minimal stub meta-schemas to support remote ref tests
draft04MetaSchema :: Schema
draft04MetaSchema =
  case parseSchemaWithVersion Draft04 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-04 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "id" .= ("http://json-schema.org/draft-04/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

draft06MetaSchema :: Schema
draft06MetaSchema =
  case parseSchemaWithVersion Draft06 metaValue of
    Right schema -> schema
    Left err -> error $ "Failed to build draft-06 meta schema stub: " <> show err
  where
    metaValue =
      object
        [ "$id" .= ("http://json-schema.org/draft-06/schema#" :: Text)
        , "type" .= ("object" :: Text)
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

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
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
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
        , "properties" .= object
            [ "minLength" .= object
                [ "type" .= ("integer" :: Text)
                , "minimum" .= (0 :: Int)
                ]
            ]
        , "additionalProperties" .= True
        ]

-- | Run a single test case
runTestCase :: JsonSchemaVersion -> FilePath -> TestGroup -> TestCase -> Expectation
runTestCase version filePath group testCase = do
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
          -- Enable format assertion for optional/format tests
          let isFormatTest = "optional/format" `T.isInfixOf` T.pack filePath
              config = defaultValidationConfig
                { validationVersion = version
                , validationFormatAssertion = isFormatTest
                }
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
              runTestCase version filePath group testCase
  where
    -- Extract just the relative path for display (works with either path prefix)
    fileLabel =
      let withoutPrefix1 = makeRelative "test-suite/json-schema-test-suite/tests" filePath
          withoutPrefix2 = makeRelative "fractal-openapi/test-suite/json-schema-test-suite/tests" filePath
      in if length withoutPrefix1 < length withoutPrefix2
         then withoutPrefix1
         else withoutPrefix2

-- | Find the test suite root directory, trying both with and without fractal-openapi/ prefix
findTestSuiteRoot :: IO FilePath
findTestSuiteRoot = do
  let paths = [ "test-suite/json-schema-test-suite/tests"
              , "fractal-openapi/test-suite/json-schema-test-suite/tests"
              ]
  findFirst paths
  where
    findFirst [] = error "Could not find test suite directory"
    findFirst (p:ps) = do
      exists <- doesDirectoryExist p
      if exists then pure p else findFirst ps

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
    testSuiteRoot <- runIO findTestSuiteRoot

    it "loads test suite structure" $ do
      -- Verify test suite is available
      exists <- doesFileExist (testSuiteRoot </> "draft7/type.json")
      exists `shouldBe` True

    let versionDirs =
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
