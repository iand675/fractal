{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Fractal.Schema.ClientSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Avro
import Data.Avro.Deriving
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Hasql.Connection as HC
import qualified Hasql.Pool as HP
import qualified Hasql.TH as HTH
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Servant.Client (runClientM, mkClientEnv, ClientEnv, ClientM, BaseUrl(..), Scheme(..))
import Control.Monad.Except (runExceptT)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Network.Wai.Handler.Warp (run)
import Fractal.Schema.Registry (app, schemaRegistryServer)
import Fractal.Schema.Backend.PostgreSQL (HasqlStore(..), runHasqlStore)
import qualified Hasql.Session as HS

import Fractal.Schema.Client
import Fractal.Schema.Types hiding (SchemaRegistryRoutes(..))
import Fractal.Schema.Compatibility.Avro hiding (CompatibilityError, TypeMismatch)

-- Test data
testSubject :: SubjectName
testSubject = SubjectName "test-subject"

testSchema :: Data.Avro.Schema
testSchema = $(makeSchemaFromByteString [r|
  {
    "name": "TestRecord",
    "type": "record",
    "fields": [
      {"name": "id", "type": "int"},
      {"name": "name", "type": "string"}
    ]
  }
|])

testSchemaV2 :: Data.Avro.Schema
testSchemaV2 = $(makeSchemaFromByteString [r|
  {
    "name": "TestRecord",
    "type": "record",
    "fields": [
      {"name": "id", "type": "int"},
      {"name": "name", "type": "string"},
      {"name": "email", "type": "string", "default": "default@example.com"}
    ]
  }
|])


-- Database schema setup
setupDatabaseSchema :: HC.Connection -> IO ()
setupDatabaseSchema conn = do
  let createTables :: HST.Statement () ()
      createTables = HST.Statement sql encoder decoder True
        where
          sql = "CREATE TABLE IF NOT EXISTS schemas (\
                \  id SERIAL PRIMARY KEY,\
                \  schema TEXT NOT NULL,\
                \  schema_type TEXT,\
                \  hash TEXT NOT NULL UNIQUE,\
                \  created_at TIMESTAMPTZ DEFAULT NOW()\
                \)"
          encoder = HE.noParams
          decoder = HD.noResult

  result1 <- HS.run (HS.statement () createTables) conn
  case result1 of
    Left err -> error $ "Failed to create schemas table: " ++ show err
    Right _ -> pure ()

  let createSubjectVersions :: HST.Statement () ()
      createSubjectVersions = HST.Statement sql encoder decoder True
        where
          sql = "CREATE TABLE IF NOT EXISTS subject_versions (\
                \  subject TEXT NOT NULL,\
                \  version INT NOT NULL,\
                \  schema_id INT NOT NULL REFERENCES schemas(id),\
                \  deleted BOOLEAN DEFAULT FALSE,\
                \  created_at TIMESTAMPTZ DEFAULT NOW(),\
                \  PRIMARY KEY (subject, version)\
                \)"
          encoder = HE.noParams
          decoder = HD.noResult

  result2 <- HS.run (HS.statement () createSubjectVersions) conn
  case result2 of
    Left err -> error $ "Failed to create subject_versions table: " ++ show err
    Right _ -> pure ()

  let createIndexes :: HST.Statement () ()
      createIndexes = HST.Statement sql encoder decoder True
        where
          sql = "CREATE INDEX IF NOT EXISTS idx_subject_versions_schema_id ON subject_versions(schema_id)"
          encoder = HE.noParams
          decoder = HD.noResult

  result3 <- HS.run (HS.statement () createIndexes) conn
  case result3 of
    Left err -> error $ "Failed to create schema_id index: " ++ show err
    Right _ -> pure ()

  let createDeletedIndex :: HST.Statement () ()
      createDeletedIndex = HST.Statement sql encoder decoder True
        where
          sql = "CREATE INDEX IF NOT EXISTS idx_subject_versions_deleted ON subject_versions(subject, deleted) WHERE NOT deleted"
          encoder = HE.noParams
          decoder = HD.noResult

  result4 <- HS.run (HS.statement () createDeletedIndex) conn
  case result4 of
    Left err -> error $ "Failed to create deleted index: " ++ show err
    Right _ -> pure ()

  let createConfigs :: HST.Statement () ()
      createConfigs = HST.Statement sql encoder decoder True
        where
          sql = "CREATE TABLE IF NOT EXISTS configs (\
                \  subject TEXT,\
                \  compatibility TEXT NOT NULL,\
                \  updated_at TIMESTAMPTZ DEFAULT NOW(),\
                \  UNIQUE (subject)\
                \)"
          encoder = HE.noParams
          decoder = HD.noResult

  result5 <- HS.run (HS.statement () createConfigs) conn
  case result5 of
    Left err -> error $ "Failed to create configs table: " ++ show err
    Right _ -> pure ()

  let createModes :: HST.Statement () ()
      createModes = HST.Statement sql encoder decoder True
        where
          sql = "CREATE TABLE IF NOT EXISTS modes (\
                \  subject TEXT,\
                \  mode TEXT NOT NULL,\
                \  updated_at TIMESTAMPTZ DEFAULT NOW(),\
                \  UNIQUE (subject)\
                \)"
          encoder = HE.noParams
          decoder = HD.noResult

  result5 <- HS.run (HS.statement () createModes) conn
  case result5 of
    Left err -> error $ "Failed to create modes table: " ++ show err
    Right _ -> pure ()

-- Test setup
withTestEnvironment :: (Manager -> HC.Connection -> IO a) -> IO a
withTestEnvironment action = do
  -- Create HTTP manager
  manager <- newManager tlsManagerSettings

  -- Create database connection for server
  bracket (HC.acquire "host=localhost port=5432 dbname=schema_registry_test user=postgres")
          (\conn -> case conn of
              Right c -> HC.release c
              Left _ -> pure ())
          $ \conn -> case conn of
              Right serverConn -> do
                -- Set up database schema
                setupDatabaseSchema serverConn

                -- Start Schema Registry server in a separate thread
                serverStarted <- newEmptyMVar
                serverThread <- forkIO $ do
                  putMVar serverStarted ()
                  run 8081 $ app serverConn

                -- Wait for server to start
                takeMVar serverStarted

                -- Create database connection for client
                bracket (HC.acquire "host=localhost port=5432 dbname=schema_registry_test user=postgres")
                        (\conn -> case conn of
                            Right c -> HC.release c
                            Left _ -> pure ())
                        $ \conn -> case conn of
                            Right clientConn -> do
                              result <- action manager clientConn
                              killThread serverThread  -- Clean up server thread
                              pure result
                            Left err -> do
                              killThread serverThread  -- Clean up server thread
                              error $ "Failed to acquire client connection: " ++ show err
              Left err -> error $ "Failed to acquire server connection: " ++ show err

-- Helper function to create client environment
mkTestClientEnv :: Manager -> ClientEnv
mkTestClientEnv manager = mkClientEnv manager baseUrl
  where
    baseUrl = BaseUrl Http "localhost" 8081 ""

-- Helper function to run client actions
runTestClient :: Manager -> ClientM a -> IO a
runTestClient manager action = do
  let clientEnv = mkTestClientEnv manager
  result <- runClientM action clientEnv
  case result of
    Right a -> pure a
    Left err -> error $ "Client error: " ++ show err

-- Helper function to convert ByteString to Text
bsToText :: LBS.ByteString -> Text
bsToText = T.decodeUtf8 . LBS.toStrict

-- Helper function to convert SubjectName to Text
subjectNameToText :: SubjectName -> Text
subjectNameToText (SubjectName name) = name

spec :: Spec
spec = describe "Schema Registry Client" $ do
  describe "Basic Schema Operations" $ do
    it "can register and retrieve a schema" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        registered <- runTestClient manager $ registerSchema testSubject schema
        registered `shouldSatisfy` \r -> r.id /= SchemaId 0

        -- Retrieve schema
        retrieved <- runTestClient manager $ getSchemaById registered.id
        retrieved.schema `shouldBe` schema.schema
        retrieved.schemaType `shouldBe` schema.schemaType

    it "can list subjects" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema to create a subject
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- List subjects
        subjects <- runTestClient manager getSubjects
        subjects `shouldContain` [subjectNameToText testSubject]

    it "can get subject versions" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Get versions
        versions <- runTestClient manager $ getSubjectVersions testSubject
        versions `shouldBe` [Version 1]

  describe "Schema Compatibility" $ do
    it "checks backward compatibility" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register initial schema
        let schema1 = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema1

        -- Check compatibility of compatible schema
        let schema2 = Schema
              { schema = bsToText $ Aeson.encode testSchemaV2
              , schemaType = Just AVRO
              , references = Nothing
              }
        result <- runTestClient manager $ checkCompatibilityLatest testSubject schema2
        result `shouldBe` CompatibilityCheckResult True Nothing

        -- Check compatibility of incompatible schema
        let incompatibleSchema = Schema
              { schema = bsToText $ Aeson.encode $(makeSchemaFromByteString [r|
                {
                  "name": "TestRecord",
                  "type": "record",
                  "fields": [
                    {"name": "id", "type": "string"}
                  ]
                }
              |])
              , schemaType = Just AVRO
              , references = Nothing
              }
        result' <- runTestClient manager $ checkCompatibilityLatest testSubject incompatibleSchema
        result' `shouldBe` CompatibilityCheckResult False (Just [CompatibilityError TypeMismatch "" "Schema types do not match"])

  describe "Configuration Operations" $ do
    it "can get and update global compatibility" $ do
      withTestEnvironment $ \manager conn -> do
        -- Get initial config
        initialConfig <- runTestClient manager getGlobalConfig
        initialConfig.compatibilityLevel `shouldBe` BACKWARD

        -- Update config
        let newConfig = ConfigUpdateRequest FULL
        updatedConfig <- runTestClient manager $ updateGlobalConfig newConfig
        updatedConfig `shouldBe` newConfig

        -- Verify update
        finalConfig <- runTestClient manager getGlobalConfig
        finalConfig.compatibilityLevel `shouldBe` FULL

    it "can get and update subject compatibility" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema to create the subject
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Get initial config
        initialConfig <- runTestClient manager $ getSubjectConfig testSubject
        initialConfig.compatibilityLevel `shouldBe` BACKWARD

        -- Update config
        let newConfig = ConfigUpdateRequest FORWARD
        updatedConfig <- runTestClient manager $ updateSubjectConfig testSubject newConfig
        updatedConfig `shouldBe` newConfig

        -- Verify update
        finalConfig <- runTestClient manager $ getSubjectConfig testSubject
        finalConfig.compatibilityLevel `shouldBe` FORWARD

  describe "Cleanup Operations" $ do
    it "can delete subject versions" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Delete version
        deleted <- runTestClient manager $ deleteSubjectVersion testSubject (Version 1)
        deleted `shouldBe` Version 1

        -- Verify deletion
        versions <- runTestClient manager $ getSubjectVersions testSubject
        versions `shouldBe` []

    it "can delete entire subject" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Delete subject
        deleted <- runTestClient manager $ deleteSubject testSubject
        deleted `shouldBe` [Version 1]

        -- Verify deletion
        subjects <- runTestClient manager getSubjects
        subjects `shouldNotContain` [subjectNameToText testSubject]
