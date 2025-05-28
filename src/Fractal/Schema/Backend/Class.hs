module Fractal.Schema.Backend.Class where

import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Data.Int (Int32, Int64)
import GHC.Generics (Generic)
import Data.Vector (Vector)

-- Import from the types module
import Fractal.Schema.Types
  ( SubjectName(..)
  , SchemaId(..)
  , Version(..)
  , Schema(..)
  , SchemaInfo(..)
  , SchemaType(..)
  , CompatibilityLevel(..)
  , ConfigResponse(..)
  , ModeResponse(..)
  )

-- Store-specific types
data SchemaRecord = SchemaRecord
  { srId :: SchemaId
  , srSchema :: Text
  , srSchemaType :: Maybe SchemaType
  , srHash :: Text  -- For deduplication
  } deriving (Show, Eq, Generic)

data SubjectVersionRecord = SubjectVersionRecord
  { svrSubject :: SubjectName
  , svrVersion :: Version
  , svrSchemaId :: SchemaId
  , svrDeleted :: Bool
  } deriving (Show, Eq, Generic)

data ConfigRecord = ConfigRecord
  { crSubject :: Maybe SubjectName  -- Nothing for global config
  , crCompatibility :: CompatibilityLevel
  } deriving (Show, Eq, Generic)

data ModeRecord = ModeRecord
  { mrSubject :: Maybe SubjectName  -- Nothing for global mode
  , mrMode :: Text
  } deriving (Show, Eq, Generic)

-- Store errors
data StoreError
  = SchemaNotFound SchemaId
  | SubjectNotFound SubjectName
  | VersionNotFound SubjectName Version
  | SchemaAlreadyExists
  | DatabaseError Text
  deriving (Show, Eq)

-- Generic store interface
class Monad m => SchemaStore m where
  -- Schema operations
  getSchema :: SchemaId -> m (Either StoreError SchemaRecord)
  findSchemaByHash :: Text -> m (Maybe SchemaRecord)
  insertSchema :: Text -> Maybe SchemaType -> Text -> m (Either StoreError SchemaId)
  getAllSchemaTypes :: m (Vector SchemaType)

  -- Subject operations
  listSubjects :: m (Vector SubjectName)
  getSubjectVersions :: SubjectName -> m (Vector Version)
  getSubjectVersion :: SubjectName -> Version -> m (Either StoreError (SchemaId, Bool))
  getLatestVersion :: SubjectName -> m (Either StoreError Version)
  registerSchemaVersion :: SubjectName -> SchemaId -> m (Either StoreError Version)
  softDeleteSubject :: SubjectName -> m (Vector Version)
  softDeleteVersion :: SubjectName -> Version -> m (Either StoreError ())

  -- Config operations
  getGlobalCompatibility :: m CompatibilityLevel
  setGlobalCompatibility :: CompatibilityLevel -> m ()
  getSubjectCompatibility :: SubjectName -> m (Maybe CompatibilityLevel)
  setSubjectCompatibility :: SubjectName -> CompatibilityLevel -> m ()
  deleteSubjectCompatibility :: SubjectName -> m ()

  -- Mode operations
  getGlobalMode :: m Text
  setGlobalMode :: Text -> m ()
  getSubjectMode :: SubjectName -> m (Maybe Text)
  setSubjectMode :: SubjectName -> Text -> m ()
  deleteSubjectMode :: SubjectName -> m ()
