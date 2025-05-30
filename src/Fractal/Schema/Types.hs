{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fractal.Schema.Types where

import Data.Aeson
import Data.Text (Text)
import Data.Proxy
import GHC.Generics
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Int (Int64)

-- Domain Types

newtype SubjectName = SubjectName Text
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype SchemaId = SchemaId Int64
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

newtype Version = Version Int64
  deriving newtype (Eq, Show, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)

data SchemaType = AVRO | JSON | PROTOBUF
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | These levels determine how schema evolution is handled and what changes are allowed
-- between different versions of a schema.
--
-- == Compatibility Types
--
-- * __BACKWARD__: A new schema is backward compatible if it can read data written with the old schema.
--   This is the most common compatibility mode. You can:
--     * Add fields with defaults
--     * Remove fields (the reader will ignore them)
--     * Promote types: int → long/float/double, long → float/double, float → double, string ↔ bytes
--
-- * __BACKWARD_TRANSITIVE__: Like BACKWARD, but checks compatibility across all historical versions,
--   not just the immediate predecessor. This ensures that any new schema can read data written with
--   any previous version.
--
-- * __FORWARD__: A new schema is forward compatible if the old schema can read data written with the new schema.
--   You can:
--     * Remove fields
--     * Add fields only if the old schema has defaults for them
--     * Demote types where supported
--
-- * __FORWARD_TRANSITIVE__: Like FORWARD, but checks compatibility across all historical versions,
--   not just the immediate predecessor. This ensures that any old schema can read data written with
--   any newer version.
--
-- * __FULL__: A schema is fully compatible if it is both backward and forward compatible.
--   This is the most restrictive mode. You can:
--     * Add fields with defaults
--     * Remove fields with defaults
--     * Promote/demote types where supported in both directions
--
-- * __FULL_TRANSITIVE__: Like FULL, but checks compatibility across all historical versions,
--   not just the immediate predecessor. This ensures that any schema can read data written with
--   any other version.
--
-- * __NONE__: No compatibility checks are performed. Any schema changes are allowed.
--
-- == Examples
--
-- Backward compatible changes:
-- @
-- -- Old schema
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"}
-- ]}
--
-- -- New schema (backward compatible)
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"},
--   {"name": "name", "type": "string", "default": "Unknown"}
-- ]}
-- @
--
-- Forward compatible changes:
-- @
-- -- Old schema
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"},
--   {"name": "email", "type": "string"}
-- ]}
--
-- -- New schema (forward compatible)
-- {"type": "record", "name": "User", "fields": [
--   {"name": "id", "type": "int"}
-- ]}
-- @
data CompatibilityLevel
  = BACKWARD
  | BACKWARD_TRANSITIVE
  | FORWARD
  | FORWARD_TRANSITIVE
  | FULL
  | FULL_TRANSITIVE
  | NONE
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Schema = Schema
  { schema :: Text
  , schemaType :: Maybe SchemaType
  , references :: Maybe [SchemaReference]
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SchemaReference = SchemaReference
  { name :: Text
  , subject :: Text
  , version :: Int
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data RegisteredSchema = RegisteredSchema
  { id :: SchemaId
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SchemaInfo = SchemaInfo
  { subject :: Text
  , id :: SchemaId
  , version :: Version
  , schemaType :: Maybe SchemaType
  , schema :: Text
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data CompatibilityCheckResult = CompatibilityCheckResult
  { is_compatible :: Bool
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ConfigUpdateRequest = ConfigUpdateRequest
  { compatibility :: CompatibilityLevel
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ConfigResponse = ConfigResponse
  { compatibilityLevel :: CompatibilityLevel
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ErrorResponse = ErrorResponse
  { error_code :: Int
  , message :: Text
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data SubjectVersion = SubjectVersion
  { subject :: Text
  , version :: Version
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ModeResponse = ModeResponse
  { mode :: Text
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ModeUpdateRequest = ModeUpdateRequest
  { mode :: Text
  } deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- API Routes

data SchemaRegistryRoutes route = SchemaRegistryRoutes
  { -- Schemas
    getSchemaById :: route :- "schemas" :> "ids" :> Capture "id" SchemaId :> Get '[JSON] Schema
  , getSchemaTypes :: route :- "schemas" :> "types" :> Get '[JSON] [SchemaType]
  , getSchemaVersions :: route :- "schemas" :> "ids" :> Capture "id" SchemaId :> "versions" :> Get '[JSON] [SubjectVersion]

    -- Subjects
  , getSubjects :: route :- "subjects" :> Get '[JSON] [Text]
  , getSubjectVersions :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> Get '[JSON] [Version]
  , deleteSubject :: route :- "subjects" :> Capture "subject" SubjectName :> Delete '[JSON] [Version]

    -- Subject versions
  , getSubjectVersion :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> Capture "version" Version :> Get '[JSON] SchemaInfo
  , getLatestVersion :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> "latest" :> Get '[JSON] SchemaInfo
  , getSchemaBySubjectVersion :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> Capture "version" Version :> "schema" :> Get '[JSON] Schema
  , registerSchema :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> ReqBody '[JSON] Schema :> Post '[JSON] RegisteredSchema
  , deleteSubjectVersion :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> Capture "version" Version :> Delete '[JSON] Version
  , getReferencedBy :: route :- "subjects" :> Capture "subject" SubjectName :> "versions" :> Capture "version" Version :> "referencedby" :> Get '[JSON] [SchemaInfo]

    -- Compatibility
  , checkCompatibility :: route :- "compatibility" :> "subjects" :> Capture "subject" SubjectName :> "versions" :> Capture "version" Version :> ReqBody '[JSON] Schema :> Post '[JSON] CompatibilityCheckResult
  , checkCompatibilityLatest :: route :- "compatibility" :> "subjects" :> Capture "subject" SubjectName :> "versions" :> "latest" :> ReqBody '[JSON] Schema :> Post '[JSON] CompatibilityCheckResult

    -- Config
  , getGlobalConfig :: route :- "config" :> Get '[JSON] ConfigResponse
  , updateGlobalConfig :: route :- "config" :> ReqBody '[JSON] ConfigUpdateRequest :> Put '[JSON] ConfigUpdateRequest
  , getSubjectConfig :: route :- "config" :> Capture "subject" SubjectName :> Get '[JSON] ConfigResponse
  , updateSubjectConfig :: route :- "config" :> Capture "subject" SubjectName :> ReqBody '[JSON] ConfigUpdateRequest :> Put '[JSON] ConfigUpdateRequest
  , deleteSubjectConfig :: route :- "config" :> Capture "subject" SubjectName :> Delete '[JSON] ConfigResponse

    -- Mode
  , getMode :: route :- "mode" :> Get '[JSON] ModeResponse
  , updateMode :: route :- "mode" :> ReqBody '[JSON] ModeUpdateRequest :> Put '[JSON] ModeUpdateRequest
  , getSubjectMode :: route :- "mode" :> Capture "subject" SubjectName :> Get '[JSON] ModeResponse
  , updateSubjectMode :: route :- "mode" :> Capture "subject" SubjectName :> ReqBody '[JSON] ModeUpdateRequest :> Put '[JSON] ModeUpdateRequest
  , deleteSubjectMode :: route :- "mode" :> Capture "subject" SubjectName :> Delete '[JSON] ModeResponse
  } deriving (Generic)

-- API Type
type API = ToServantApi SchemaRegistryRoutes
