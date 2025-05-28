{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}

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


-- Server Implementation using PostgreSQL backend
schemaRegistryServer :: SchemaRegistryRoutes AsServer
schemaRegistryServer = SchemaRegistryRoutes
  { getSchemaById = \schemaId -> do
      result <- runHasqlStore conn $ Class.getSchema schemaId
      case result of
        Right record -> pure $ Schema (Class.srSchema record) (Class.srSchemaType record) Nothing
        Left err -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to fetch schema" }

  , getSchemaTypes = do
      types <- runHasqlStore conn Class.getAllSchemaTypes
      pure $ V.toList types

  , getSchemaVersions = \schemaId -> do
      -- This would need additional implementation in the store
      throwError $ err501 { errBody = encode $ ErrorResponse 501 "Not implemented" }

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
      let hash = "TODO" -- TODO: Implement schema hashing
      existingSchema <- runHasqlStore conn $ Class.findSchemaByHash hash
      case existingSchema of
        Just record -> do
          -- Schema exists, register new version
          versionResult <- runHasqlStore conn $ Class.registerSchemaVersion (SubjectName subject) (Class.srId record)
          case versionResult of
            Right version -> pure $ RegisteredSchema (Class.srId record)
            Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to register schema version" }
        Nothing -> do
          -- New schema, insert and register
          schemaResult <- runHasqlStore conn $ Class.insertSchema hash schema.schemaType schema.schema
          case schemaResult of
            Right schemaId -> do
              versionResult <- runHasqlStore conn $ Class.registerSchemaVersion (SubjectName subject) schemaId
              case versionResult of
                Right _ -> pure $ RegisteredSchema schemaId
                Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to register schema version" }
            Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to insert schema" }

  , deleteSubjectVersion = \(SubjectName subject) (Version version) -> do
      result <- runHasqlStore conn $ Class.softDeleteVersion (SubjectName subject) (Version version)
      case result of
        Right _ -> pure $ Version version
        Left _ -> throwError $ err500 { errBody = encode $ ErrorResponse 500 "Failed to delete version" }

  , getReferencedBy = \_ _ -> do
      -- This would need additional implementation in the store
      throwError $ err501 { errBody = encode $ ErrorResponse 501 "Not implemented" }

  , checkCompatibility = \_ _ _ -> do
      -- This would need additional implementation in the store
      throwError $ err501 { errBody = encode $ ErrorResponse 501 "Not implemented" }

  , checkCompatibilityLatest = \_ _ -> do
      -- This would need additional implementation in the store
      throwError $ err501 { errBody = encode $ ErrorResponse 501 "Not implemented" }

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
  where
    conn = undefined -- This needs to be provided from the application configuration

-- Application
app :: Application
app = genericServe schemaRegistryServer

-- Main
main :: IO ()
main = do
  putStrLn "Starting Schema Registry on port 8081"
  run 8081 app
