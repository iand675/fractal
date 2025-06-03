{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Fractal.Schema.Registry where

import Data.Aeson (encode)
import Data.Text (Text)
import qualified Data.Vector as V
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Servant.Server (err404, err500, err501)
import Network.Wai.Handler.Warp (run)
import Fractal.Schema.Types
import Fractal.Schema.Backend.PostgreSQL
import qualified Fractal.Schema.Backend.Class as Class
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.TH as HTH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Exception (bracket)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Crypto.Hash (hashWith, SHA256(..))
import Data.ByteArray (convert)
import qualified Data.Text as T

-- Database schema setup
setupDatabaseSchema :: HC.Connection -> IO ()
setupDatabaseSchema conn = do
  let createTables = HST.Statement
        "CREATE TABLE IF NOT EXISTS schemas (id SERIAL PRIMARY KEY, schema TEXT NOT NULL, schema_type TEXT, hash TEXT NOT NULL UNIQUE, created_at TIMESTAMPTZ DEFAULT NOW())"
        HE.noParams
        HD.noResult
        True
  result1 <- HS.run (HS.statement () createTables) conn
  case result1 of
    Left err -> error $ "Failed to create schemas table: " ++ show err
    Right _ -> pure ()

  let createSubjectVersions = HST.Statement
        "CREATE TABLE IF NOT EXISTS subject_versions (subject TEXT NOT NULL, version INT NOT NULL, schema_id INT NOT NULL REFERENCES schemas(id), deleted BOOLEAN DEFAULT FALSE, created_at TIMESTAMPTZ DEFAULT NOW(), PRIMARY KEY (subject, version))"
        HE.noParams
        HD.noResult
        True
  result2 <- HS.run (HS.statement () createSubjectVersions) conn
  case result2 of
    Left err -> error $ "Failed to create subject_versions table: " ++ show err
    Right _ -> pure ()

  let createIndexes = HST.Statement
        "CREATE INDEX IF NOT EXISTS idx_subject_versions_schema_id ON subject_versions(schema_id); CREATE INDEX IF NOT EXISTS idx_subject_versions_deleted ON subject_versions(subject, deleted) WHERE NOT deleted;"
        HE.noParams
        HD.noResult
        True
  result3 <- HS.run (HS.statement () createIndexes) conn
  case result3 of
    Left err -> error $ "Failed to create indexes: " ++ show err
    Right _ -> pure ()

  let createConfigs = HST.Statement
        "CREATE TABLE IF NOT EXISTS configs (subject TEXT, compatibility TEXT NOT NULL, updated_at TIMESTAMPTZ DEFAULT NOW(), UNIQUE (subject))"
        HE.noParams
        HD.noResult
        True
  result4 <- HS.run (HS.statement () createConfigs) conn
  case result4 of
    Left err -> error $ "Failed to create configs table: " ++ show err
    Right _ -> pure ()

  let createModes = HST.Statement
        "CREATE TABLE IF NOT EXISTS modes (subject TEXT, mode TEXT NOT NULL, updated_at TIMESTAMPTZ DEFAULT NOW(), UNIQUE (subject))"
        HE.noParams
        HD.noResult
        True
  result5 <- HS.run (HS.statement () createModes) conn
  case result5 of
    Left err -> error $ "Failed to create modes table: " ++ show err
    Right _ -> pure ()

-- Server Implementation using PostgreSQL backend
schemaRegistryServer :: HC.Connection -> SchemaRegistryRoutes AsServer
schemaRegistryServer conn = SchemaRegistryRoutes
  { getSchemaById = \schemaId -> do
      result <- runHasqlStore conn $ Class.getSchema schemaId
      case result of
        Right record -> pure $ Schema (Class.srSchema record) (Class.srSchemaType record) Nothing
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema" }

  , getSchemaTypes = do
      types <- runHasqlStore conn Class.getAllSchemaTypes
      pure $ V.toList types

  , getSchemaVersions = \schemaId -> do
      result <- runHasqlStore conn $ Class.getSchemaVersions schemaId
      case result of
        Right versions -> pure $ V.toList versions
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema versions" }

  , getSubjects = do
      subjects <- runHasqlStore conn Class.listSubjects
      pure ((\(SubjectName s) -> s) <$> V.toList subjects)

  , getSubjectVersions = \(SubjectName subject) -> do
      versions <- runHasqlStore conn $ Class.getSubjectVersions (SubjectName subject)
      pure $ V.toList versions

  , deleteSubject = \(SubjectName subject) -> do
      versions <- runHasqlStore conn $ Class.softDeleteSubject (SubjectName subject)
      pure $ V.toList versions

  , getSubjectVersion = \(SubjectName subject) (Version version) -> do
      result <- runHasqlStore conn $ Class.getSubjectVersion (SubjectName subject) (Version version)
      case result of
        Right (schemaId, deleted) -> do
          if deleted
            then throwError $ err404 { errBody = encode $ ErrorResponse 404 "Version not found" }
            else do
              schemaResult <- runHasqlStore conn $ Class.getSchema schemaId
              case schemaResult of
                Right record -> pure $ SchemaInfo
                  subject
                  schemaId
                  (Version version)
                  (Class.srSchemaType record)
                  (Class.srSchema record)
                Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema" }
        Left (Class.VersionNotFound _ _) -> throwError $ err404 { errBody = encode $ ErrorResponse 404 "Version not found" }
        Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch version" }

  , getLatestVersion = \(SubjectName subject) -> do
      result <- runHasqlStore conn $ Class.getLatestVersion (SubjectName subject)
      case result of
        Right version -> do
          versionResult <- runHasqlStore conn $ Class.getSubjectVersion (SubjectName subject) version
          case versionResult of
            Right (schemaId, _) -> do
              schemaResult <- runHasqlStore conn $ Class.getSchema schemaId
              case schemaResult of
                Right record -> pure $ SchemaInfo
                  subject
                  schemaId
                  version
                  (Class.srSchemaType record)
                  (Class.srSchema record)
                Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema" }
            Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch version" }
        Left (Class.SubjectNotFound _) -> throwError $ err404 { errBody = encode $ ErrorResponse 404 "Subject not found" }
        Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch latest version" }

  , getSchemaBySubjectVersion = \(SubjectName subject) (Version version) -> do
      result <- runHasqlStore conn $ Class.getSubjectVersion (SubjectName subject) (Version version)
      case result of
        Right (schemaId, deleted) -> do
          if deleted
            then throwError $ err404 { errBody = encode $ ErrorResponse 404 "Version not found" }
            else do
              schemaResult <- runHasqlStore conn $ Class.getSchema schemaId
              case schemaResult of
                Right record -> pure $ Schema (Class.srSchema record) (Class.srSchemaType record) Nothing
                Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema" }
        Left (Class.VersionNotFound _ _) -> throwError $ err404 { errBody = encode $ ErrorResponse 404 "Version not found" }
        Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch version" }

  , registerSchema = \(SubjectName subject) schema -> do
      -- First check if schema already exists
      let hash = T.pack $ BS8.unpack $ convert $ hashWith SHA256 $ LBS.toStrict $ encode schema
      existingSchema <- runHasqlStore conn $ Class.findSchemaByHash hash
      case existingSchema of
        Just record -> do
          -- Schema exists, register new version
          versionResult <- runHasqlStore conn $ Class.registerSchemaVersion (SubjectName subject) (Class.srId record)
          case versionResult of
            Right version -> pure $ RegisteredSchema (Class.srId record)
            Left err -> do
              liftIO $ putStrLn $ "Failed to register schema version for subject " ++ T.unpack subject ++ ": " ++ show err
              throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to register schema version" }
        Nothing -> do
          -- New schema, insert and register
          schemaResult <- runHasqlStore conn $ Class.insertSchema hash schema.schemaType schema.schema
          case schemaResult of
            Right schemaId -> do
              versionResult <- runHasqlStore conn $ Class.registerSchemaVersion (SubjectName subject) schemaId
              case versionResult of
                Right _ -> pure $ RegisteredSchema schemaId
                Left err -> do
                  liftIO $ putStrLn $ "Failed to register schema version for subject " ++ T.unpack subject ++ ": " ++ show err
                  throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to register schema version" }
            Left err -> do
              liftIO $ putStrLn $ "Failed to insert schema for subject " ++ T.unpack subject ++ ": " ++ show err
              throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to insert schema" }

  , deleteSubjectVersion = \(SubjectName subject) (Version version) -> do
      result <- runHasqlStore conn $ Class.softDeleteVersion (SubjectName subject) (Version version)
      case result of
        Right _ -> pure $ Version version
        Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to delete version" }

  , getReferencedBy = \(SubjectName subject) (Version version) -> do
      result <- runHasqlStore conn $ Class.getReferencedBy (SubjectName subject) (Version version)
      case result of
        Right refs -> pure $ V.toList refs
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch references" }

  , checkCompatibility = \(SubjectName subject) (Version version) schema -> do
      result <- runHasqlStore conn $ Class.checkCompatibility (SubjectName subject) (Version version) schema
      case result of
        Right (CompatibilityCheckResult is_compatible errors) -> pure $ CompatibilityCheckResult is_compatible errors
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to check compatibility" }

  , checkCompatibilityLatest = \(SubjectName subject) schema -> do
      result <- runHasqlStore conn $ Class.checkCompatibilityLatest (SubjectName subject) schema
      case result of
        Right (CompatibilityCheckResult is_compatible errors) -> pure $ CompatibilityCheckResult is_compatible errors
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to check compatibility" }

  , getGlobalConfig = do
      compat <- runHasqlStore conn Class.getGlobalCompatibility
      pure $ ConfigResponse compat

  , updateGlobalConfig = \req -> do
      runHasqlStore conn $ Class.setGlobalCompatibility (compatibility req)
      pure req

  , getSubjectConfig = \(SubjectName subject) -> do
      compat <- runHasqlStore conn $ Class.getSubjectCompatibility (SubjectName subject)
      case compat of
        Just c -> pure $ ConfigResponse c
        Nothing -> do
          globalCompat <- runHasqlStore conn Class.getGlobalCompatibility
          pure $ ConfigResponse globalCompat

  , updateSubjectConfig = \(SubjectName subject) req -> do
      runHasqlStore conn $ Class.setSubjectCompatibility (SubjectName subject) (compatibility req)
      pure req

  , deleteSubjectConfig = \(SubjectName subject) -> do
      runHasqlStore conn $ Class.deleteSubjectCompatibility (SubjectName subject)
      globalCompat <- runHasqlStore conn Class.getGlobalCompatibility
      pure $ ConfigResponse globalCompat

  , getMode = do
      mode <- runHasqlStore conn Class.getGlobalMode
      pure $ ModeResponse mode

  , updateMode = \req -> do
      runHasqlStore conn $ Class.setGlobalMode req.mode
      pure req

  , getSubjectMode = \(SubjectName subject) -> do
      mode <- runHasqlStore conn $ Class.getSubjectMode (SubjectName subject)
      case mode of
        Just m -> pure $ ModeResponse m
        Nothing -> do
          globalMode <- runHasqlStore conn Class.getGlobalMode
          pure $ ModeResponse globalMode

  , updateSubjectMode = \(SubjectName subject) req -> do
      runHasqlStore conn $ Class.setSubjectMode (SubjectName subject) req.mode
      pure req

  , deleteSubjectMode = \(SubjectName subject) -> do
      runHasqlStore conn $ Class.deleteSubjectMode (SubjectName subject)
      globalMode <- runHasqlStore conn Class.getGlobalMode
      pure $ ModeResponse globalMode
  }

-- Application
app :: HC.Connection -> Application
app conn = genericServe $ schemaRegistryServer conn

-- Main
main :: IO ()
main = do
  putStrLn "Starting Schema Registry on port 8081"
  let settings = []
  bracket (HC.acquire settings)
          (\conn -> case conn of
              Right c -> HC.release c
              Left _ -> pure ())
          $ \conn -> case conn of
              Right c -> do
                setupDatabaseSchema c
                run 8081 $ app c
              Left err -> error $ "Failed to acquire connection: " ++ show err
