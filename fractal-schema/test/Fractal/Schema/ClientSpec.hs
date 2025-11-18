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
import qualified Data.Vector as V
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
import qualified Hasql.Connection.Setting as HCS

import Fractal.Schema.Client
import Fractal.Schema.Types hiding (SchemaRegistryRoutes(..))
import Fractal.Schema.Compatibility.Avro hiding (CompatibilityError, TypeMismatch)
import TestDatabase (withTestDatabase)

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


-- Test setup
-- This will automatically spin up a temporary PostgreSQL server for testing
withTestEnvironment :: (Manager -> HC.Connection -> IO a) -> IO a
withTestEnvironment action = do
  -- Create HTTP manager
  manager <- newManager tlsManagerSettings

  -- Use temporary database (automatically downloads and starts PostgreSQL if needed)
  withTestDatabase $ \conn -> do
    -- Start Schema Registry server in a separate thread
    serverStarted <- newEmptyMVar
    serverThread <- forkIO $ do
      putMVar serverStarted ()
      run 8081 $ app conn

    -- Wait for server to start
    takeMVar serverStarted

    -- Run the test action with both manager and connection
    result <- action manager conn

    -- Clean up server thread
    killThread serverThread

    pure result

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

  describe "Mode Operations" $ do
    it "can get and update global mode" $ do
      withTestEnvironment $ \manager conn -> do
        -- Get initial global mode (should be default "READWRITE")
        initialMode <- runTestClient manager getMode
        initialMode.mode `shouldBe` "READWRITE"

        -- Update global mode
        let newMode = ModeUpdateRequest "READONLY"
        updatedMode <- runTestClient manager $ updateMode newMode
        updatedMode `shouldBe` newMode

        -- Verify update
        finalMode <- runTestClient manager getMode
        finalMode.mode `shouldBe` "READONLY"

        -- Reset to READWRITE for other tests
        let resetMode = ModeUpdateRequest "READWRITE"
        _ <- runTestClient manager $ updateMode resetMode
        pure ()

    it "can manage subject-specific modes" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema first
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Get subject mode (should inherit global by default)
        initialMode <- runTestClient manager $ getSubjectMode testSubject
        initialMode.mode `shouldBe` "READWRITE"

        -- Update subject mode
        let newMode = ModeUpdateRequest "READONLY"
        updatedMode <- runTestClient manager $ updateSubjectMode testSubject newMode
        updatedMode `shouldBe` newMode

        -- Verify update
        finalMode <- runTestClient manager $ getSubjectMode testSubject
        finalMode.mode `shouldBe` "READONLY"

    it "can delete subject mode" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema

        -- Set subject mode
        let mode = ModeUpdateRequest "READONLY"
        _ <- runTestClient manager $ updateSubjectMode testSubject mode

        -- Verify mode is set
        setMode <- runTestClient manager $ getSubjectMode testSubject
        setMode.mode `shouldBe` "READONLY"

        -- Delete subject mode (returns global mode after deletion)
        deletedMode <- runTestClient manager $ deleteSubjectMode testSubject
        deletedMode.mode `shouldBe` "READWRITE"

        -- Verify mode reverts to global (READWRITE)
        finalMode <- runTestClient manager $ getSubjectMode testSubject
        finalMode.mode `shouldBe` "READWRITE"

  describe "Error Handling and Edge Cases" $ do
    it "handles schema deduplication correctly" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register same schema twice
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        result1 <- runTestClient manager $ registerSchema testSubject schema
        result2 <- runTestClient manager $ registerSchema testSubject schema

        -- Should get same schema ID back
        result1.id `shouldBe` result2.id
        -- But different versions
        result1.version `shouldBe` Version 1
        result2.version `shouldBe` Version 1 -- Same version because schema is identical

    it "handles multiple versions correctly" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register first version
        let schema1 = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        result1 <- runTestClient manager $ registerSchema testSubject schema1

        -- Register second version (evolved schema with new optional field)
        let evolvedSchema = Aeson.object
              [ "type" Aeson..= ("record" :: Text)
              , "name" Aeson..= ("User" :: Text)
              , "fields" Aeson..= Aeson.Array (V.fromList
                  [ Aeson.object
                      [ "name" Aeson..= ("name" :: Text)
                      , "type" Aeson..= ("string" :: Text)
                      ]
                  , Aeson.object
                      [ "name" Aeson..= ("age" :: Text)
                      , "type" Aeson..= ("int" :: Text)
                      ]
                  , Aeson.object  -- New field with default
                      [ "name" Aeson..= ("email" :: Text)
                      , "type" Aeson..= ("string" :: Text)
                      , "default" Aeson..= ("" :: Text)
                      ]
                  ])
              ]
        let schema2 = Schema
              { schema = bsToText $ Aeson.encode evolvedSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        result2 <- runTestClient manager $ registerSchema testSubject schema2

        -- Should get different versions
        result1.version `shouldBe` Version 1
        result2.version `shouldBe` Version 2

        -- Verify we can retrieve both versions
        versions <- runTestClient manager $ getSubjectVersions testSubject
        length versions `shouldBe` 2

    it "handles transitive compatibility checking" $ do
      withTestEnvironment $ \manager conn -> do
        -- Set to BACKWARD_TRANSITIVE
        let config = ConfigUpdateRequest BACKWARD_TRANSITIVE
        _ <- runTestClient manager $ updateSubjectConfig testSubject config

        -- Register first version
        let schema1 = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema1

        -- Add optional field (backward compatible)
        let schema2Data = Aeson.object
              [ "type" Aeson..= ("record" :: Text)
              , "name" Aeson..= ("User" :: Text)
              , "fields" Aeson..= Aeson.Array (V.fromList
                  [ Aeson.object
                      [ "name" Aeson..= ("name" :: Text)
                      , "type" Aeson..= ("string" :: Text)
                      ]
                  , Aeson.object
                      [ "name" Aeson..= ("age" :: Text)
                      , "type" Aeson..= ("int" :: Text)
                      ]
                  , Aeson.object
                      [ "name" Aeson..= ("email" :: Text)
                      , "type" Aeson..= ("string" :: Text)
                      , "default" Aeson..= ("" :: Text)
                      ]
                  ])
              ]
        let schema2 = Schema
              { schema = bsToText $ Aeson.encode schema2Data
              , schemaType = Just AVRO
              , references = Nothing
              }
        _ <- runTestClient manager $ registerSchema testSubject schema2

        -- Try to remove a field (not backward compatible)
        let schema3Data = Aeson.object
              [ "type" Aeson..= ("record" :: Text)
              , "name" Aeson..= ("User" :: Text)
              , "fields" Aeson..= Aeson.Array (V.fromList
                  [ Aeson.object
                      [ "name" Aeson..= ("name" :: Text)
                      , "type" Aeson..= ("string" :: Text)
                      ]
                  ])
              ]
        let schema3 = Schema
              { schema = bsToText $ Aeson.encode schema3Data
              , schemaType = Just AVRO
              , references = Nothing
              }

        -- This should fail compatibility check
        result <- runTestClient manager $ checkCompatibilityLatest testSubject schema3
        result.is_compatible `shouldBe` False

    it "handles schema retrieval by ID" $ do
      withTestEnvironment $ \manager conn -> do
        -- Register a schema
        let schema = Schema
              { schema = bsToText $ Aeson.encode testSchema
              , schemaType = Just AVRO
              , references = Nothing
              }
        result <- runTestClient manager $ registerSchema testSubject schema

        -- Get schema by ID
        retrieved <- runTestClient manager $ getSchemaById result.id
        retrieved `shouldBe` bsToText (Aeson.encode testSchema)
