{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Fractal.Schema.Client where

import Servant.Client
import Servant.Client.Generic
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Text (Text)

import Fractal.Schema.Types

-- | Create a new HTTP manager with TLS support
mkManager :: IO Manager
mkManager = newManager tlsManagerSettings

-- | Generate client functions from the API type
schemaRegistryClient :: SchemaRegistryRoutes (AsClientT ClientM)
schemaRegistryClient = genericClient

-- | Schema operations
getSchemaById :: SchemaId -> ClientM Schema
getSchemaById = schemaRegistryClient.getSchemaById

getSchemaTypes :: ClientM [SchemaType]
getSchemaTypes = schemaRegistryClient.getSchemaTypes

getSchemaVersions :: SchemaId -> ClientM [SubjectVersion]
getSchemaVersions = schemaRegistryClient.getSchemaVersions

-- | Subject operations
getSubjects :: ClientM [Text]
getSubjects = schemaRegistryClient.getSubjects

getSubjectVersions :: SubjectName -> ClientM [Version]
getSubjectVersions = schemaRegistryClient.getSubjectVersions

deleteSubject :: SubjectName -> ClientM [Version]
deleteSubject = schemaRegistryClient.deleteSubject

-- | Subject version operations
getSubjectVersion :: SubjectName -> Version -> ClientM SchemaInfo
getSubjectVersion = schemaRegistryClient.getSubjectVersion

getLatestVersion :: SubjectName -> ClientM SchemaInfo
getLatestVersion = schemaRegistryClient.getLatestVersion

getSchemaBySubjectVersion :: SubjectName -> Version -> ClientM Schema
getSchemaBySubjectVersion = schemaRegistryClient.getSchemaBySubjectVersion

registerSchema :: SubjectName -> Schema -> ClientM RegisteredSchema
registerSchema = schemaRegistryClient.registerSchema

deleteSubjectVersion :: SubjectName -> Version -> ClientM Version
deleteSubjectVersion = schemaRegistryClient.deleteSubjectVersion

getReferencedBy :: SubjectName -> Version -> ClientM [SchemaInfo]
getReferencedBy = schemaRegistryClient.getReferencedBy

-- | Compatibility operations
checkCompatibility :: SubjectName -> Version -> Schema -> ClientM CompatibilityCheckResult
checkCompatibility = schemaRegistryClient.checkCompatibility

checkCompatibilityLatest :: SubjectName -> Schema -> ClientM CompatibilityCheckResult
checkCompatibilityLatest = schemaRegistryClient.checkCompatibilityLatest

-- | Config operations
getGlobalConfig :: ClientM ConfigResponse
getGlobalConfig = schemaRegistryClient.getGlobalConfig

updateGlobalConfig :: ConfigUpdateRequest -> ClientM ConfigUpdateRequest
updateGlobalConfig = schemaRegistryClient.updateGlobalConfig

getSubjectConfig :: SubjectName -> ClientM ConfigResponse
getSubjectConfig = schemaRegistryClient.getSubjectConfig

updateSubjectConfig :: SubjectName -> ConfigUpdateRequest -> ClientM ConfigUpdateRequest
updateSubjectConfig = schemaRegistryClient.updateSubjectConfig

deleteSubjectConfig :: SubjectName -> ClientM ConfigResponse
deleteSubjectConfig = schemaRegistryClient.deleteSubjectConfig

-- | Mode operations
getMode :: ClientM ModeResponse
getMode = schemaRegistryClient.getMode

updateMode :: ModeUpdateRequest -> ClientM ModeUpdateRequest
updateMode = schemaRegistryClient.updateMode

getSubjectMode :: SubjectName -> ClientM ModeResponse
getSubjectMode = schemaRegistryClient.getSubjectMode

updateSubjectMode :: SubjectName -> ModeUpdateRequest -> ClientM ModeUpdateRequest
updateSubjectMode = schemaRegistryClient.updateSubjectMode

deleteSubjectMode :: SubjectName -> ClientM ModeResponse
deleteSubjectMode = schemaRegistryClient.deleteSubjectMode
