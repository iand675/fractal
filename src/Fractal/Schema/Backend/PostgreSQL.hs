{-# LANGUAGE QuasiQuotes #-}
module Fractal.Schema.Backend.PostgreSQL where
import Fractal.Schema.Backend.Class
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.TH as HTH
import Control.Monad.Reader
import Data.Functor.Contravariant
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Int (Int64)
import Fractal.Schema.Registry

-- Hasql-based store monad
newtype HasqlStore a = HasqlStore (ReaderT HC.Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HC.Connection)

runHasqlStore :: HC.Connection -> HasqlStore a -> IO a
runHasqlStore conn (HasqlStore m) = runReaderT m conn

-- Helper to run statements
runStatement :: HST.Statement a b -> a -> HasqlStore (Either StoreError b)
runStatement stmt params = do
  conn <- ask
  result <- liftIO $ HS.run (HS.statement params stmt) conn
  case result of
    Left err -> pure $ Left $ DatabaseError $ T.pack $ show err
    Right val -> pure $ Right val


decodeSchemaType :: HD.Row (Maybe SchemaType)
decodeSchemaType = HD.column $ HD.nullable $ HD.enum $ \case
  "AVRO" -> Just AVRO
  "JSON" -> Just JSON
  "PROTOBUF" -> Just PROTOBUF
  _ -> Nothing


-- Compatibility level encoding/decoding
encodeCompatibility :: HE.Params CompatibilityLevel
encodeCompatibility = HE.param $ HE.nonNullable $ HE.enum $ \case
  BACKWARD -> "BACKWARD"
  BACKWARD_TRANSITIVE -> "BACKWARD_TRANSITIVE"
  FORWARD -> "FORWARD"
  FORWARD_TRANSITIVE -> "FORWARD_TRANSITIVE"
  FULL -> "FULL"
  FULL_TRANSITIVE -> "FULL_TRANSITIVE"
  NONE -> "NONE"

decodeCompatibility :: HD.Row CompatibilityLevel
decodeCompatibility = HD.column $ HD.nonNullable $ HD.enum $ \case
  "BACKWARD" -> Just BACKWARD
  "BACKWARD_TRANSITIVE" -> Just BACKWARD_TRANSITIVE
  "FORWARD" -> Just FORWARD
  "FORWARD_TRANSITIVE" -> Just FORWARD_TRANSITIVE
  "FULL" -> Just FULL
  "FULL_TRANSITIVE" -> Just FULL_TRANSITIVE
  "NONE" -> Just NONE
  _ -> Nothing

-- SQL Statements
getSchemaStmt :: HST.Statement Int64 SchemaRecord
getSchemaStmt = HST.Statement sql encoder decoder True
  where
    sql = "SELECT id, schema, schema_type, hash \
          \FROM schemas \
          \WHERE id = $1"
    encoder = HE.param $ HE.nonNullable HE.int8
    decoder = HD.singleRow $ SchemaRecord
      <$> (SchemaId <$> HD.column (HD.nonNullable HD.int8))
      <*> HD.column (HD.nonNullable HD.text)
      <*> HD.column (HD.nullable $ HD.enum $ \case
            "AVRO" -> Just AVRO
            "JSON" -> Just JSON
            "PROTOBUF" -> Just PROTOBUF
            _ -> Nothing
          )
      <*> HD.column (HD.nonNullable HD.text)

findSchemaByHashStmt :: HST.Statement Text (Maybe SchemaRecord)
findSchemaByHashStmt = HST.Statement sql encoder decoder True
  where
    sql = "SELECT id, schema, schema_type, hash \
          \FROM schemas \
          \WHERE hash = $1"
    encoder = HE.param $ HE.nonNullable HE.text
    decoder = HD.rowMaybe $ SchemaRecord
      <$> (SchemaId <$> HD.column (HD.nonNullable HD.int8))
      <*> HD.column (HD.nonNullable HD.text)
      <*> HD.column (HD.nullable $ HD.enum $ \case
            "AVRO" -> Just AVRO
            "JSON" -> Just JSON
            "PROTOBUF" -> Just PROTOBUF
            _ -> Nothing
          )
      <*> HD.column (HD.nonNullable HD.text)

insertSchemaStmt :: HST.Statement (Text, Maybe Text, Text) Int64
insertSchemaStmt = [HTH.singletonStatement|
  INSERT INTO schemas (schema, schema_type, hash)
  VALUES ($1 :: text, $2 :: text?, $3 :: text)
  RETURNING id :: int8
  |]

listSubjectsStmt :: HST.Statement () (V.Vector SubjectName)
listSubjectsStmt = fmap SubjectName <$> [HTH.vectorStatement|
  SELECT DISTINCT subject :: text
  FROM subject_versions
  WHERE NOT deleted
  |]

getSubjectVersionsStmt :: HST.Statement Text (V.Vector Int64)
getSubjectVersionsStmt = [HTH.vectorStatement|
  SELECT version :: int8
  FROM subject_versions
  WHERE subject = $1 :: text AND NOT deleted
  ORDER BY version
  |]

getSubjectVersionStmt :: HST.Statement (Text, Int64) (Maybe (Int64, Bool))
getSubjectVersionStmt = [HTH.maybeStatement|
  SELECT schema_id :: int8, deleted :: bool
  FROM subject_versions
  WHERE subject = $1 :: text AND version = $2 :: int8
  |]

getLatestVersionStmt :: HST.Statement Text (Maybe Int64)
getLatestVersionStmt = [HTH.maybeStatement|
  SELECT MAX(version) :: int8
  FROM subject_versions
  WHERE subject = $1 :: text AND NOT deleted
  |]

insertSubjectVersionStmt :: HST.Statement (Text, Int64) Int64
insertSubjectVersionStmt = [HTH.singletonStatement|
  INSERT INTO subject_versions (subject, version, schema_id, deleted)
  VALUES (
    $1 :: text,
    COALESCE(
      (SELECT MAX(version) + 1 FROM subject_versions WHERE subject = $1 :: text),
      1
    ),
    $2 :: int8,
    false
  )
  RETURNING version :: int8
  |]

softDeleteSubjectStmt :: HST.Statement Text (V.Vector Int64)
softDeleteSubjectStmt = [HTH.vectorStatement|
  UPDATE subject_versions
  SET deleted = true
  WHERE subject = $1 :: text AND NOT deleted
  RETURNING version :: int8
  |]

softDeleteVersionStmt :: HST.Statement (Text, Int64) Int64
softDeleteVersionStmt = [HTH.singletonStatement|
  UPDATE subject_versions
  SET deleted = true
  WHERE subject = $1 :: text AND version = $2 :: int8
  RETURNING 1 :: int8
  |]

getGlobalConfigStmt :: HST.Statement () (Maybe Text)
getGlobalConfigStmt = [HTH.maybeStatement|
  SELECT compatibility :: text
  FROM configs
  WHERE subject IS NULL
  |]

setGlobalConfigStmt :: HST.Statement Text ()
setGlobalConfigStmt = [HTH.resultlessStatement|
  INSERT INTO configs (subject, compatibility)
  VALUES (NULL, $1 :: text)
  ON CONFLICT (subject) WHERE subject IS NULL
  DO UPDATE SET compatibility = EXCLUDED.compatibility
  |]

getSubjectConfigStmt :: HST.Statement Text (Maybe Text)
getSubjectConfigStmt = [HTH.maybeStatement|
  SELECT compatibility :: text
  FROM configs
  WHERE subject = $1 :: text
  |]

setSubjectConfigStmt :: HST.Statement (Text, Text) ()
setSubjectConfigStmt = [HTH.resultlessStatement|
  INSERT INTO configs (subject, compatibility)
  VALUES ($1 :: text, $2 :: text)
  ON CONFLICT (subject)
  DO UPDATE SET compatibility = EXCLUDED.compatibility
  |]

deleteSubjectConfigStmt :: HST.Statement Text ()
deleteSubjectConfigStmt = [HTH.resultlessStatement|
  DELETE FROM configs
  WHERE subject = $1 :: text
  |]

-- Schema Store instance for HasqlStore
instance SchemaStore HasqlStore where
  getSchema (SchemaId sid) = do
    result <- runStatement getSchemaStmt sid
    case result of
      Right record -> pure $ Right record
      Left err -> pure $ Left err

  findSchemaByHash hash = do
    result <- runStatement findSchemaByHashStmt hash
    case result of
      Right val -> pure val
      Left _ -> pure Nothing

  insertSchema schema schemaType hash = do
    let schemaTypeText = case schemaType of
          Just AVRO -> Just "AVRO"
          Just JSON -> Just "JSON"
          Just PROTOBUF -> Just "PROTOBUF"
          Nothing -> Nothing
    result <- runStatement insertSchemaStmt (schema, schemaTypeText, hash)
    case result of
      Right sid -> pure $ Right $ SchemaId sid
      Left err -> pure $ Left err

  getAllSchemaTypes = pure $ V.fromList [AVRO, JSON, PROTOBUF]

  listSubjects = do
    result <- runStatement listSubjectsStmt ()
    case result of
      Right subjects -> pure subjects
      Left _ -> pure mempty

  getSubjectVersions (SubjectName subject) = do
    result <- runStatement getSubjectVersionsStmt subject
    case result of
      Right versions -> pure $ fmap Version versions
      Left _ -> pure mempty

  getSubjectVersion (SubjectName subject) (Version version) = do
    result <- runStatement getSubjectVersionStmt (subject, version)
    case result of
      Right (Just (sid, deleted)) -> pure $ Right (SchemaId sid, deleted)
      Right Nothing -> pure $ Left $ VersionNotFound (SubjectName subject) (Version version)
      Left err -> pure $ Left err

  getLatestVersion (SubjectName subject) = do
    result <- runStatement getLatestVersionStmt subject
    case result of
      Right (Just v) -> pure $ Right $ Version v
      Right Nothing -> pure $ Left $ SubjectNotFound $ SubjectName subject
      Left err -> pure $ Left err

  registerSchemaVersion (SubjectName subject) (SchemaId sid) = do
    result <- runStatement insertSubjectVersionStmt (subject, sid)
    case result of
      Right v -> pure $ Right $ Version v
      Left err -> pure $ Left err

  softDeleteSubject (SubjectName subject) = do
    result <- runStatement softDeleteSubjectStmt subject
    case result of
      Right versions -> pure $ fmap Version versions
      Left _ -> pure mempty

  softDeleteVersion (SubjectName subject) (Version version) = do
    result <- runStatement softDeleteVersionStmt (subject, version)
    case result of
      Right _ -> pure $ Right ()
      Left err -> pure $ Left err

  getGlobalCompatibility = do
    result <- runStatement getGlobalConfigStmt ()
    case result of
      Right (Just compat) -> pure $ parseCompatibility compat
      _ -> pure BACKWARD  -- Default

  setGlobalCompatibility compat = do
    _ <- runStatement setGlobalConfigStmt (compatToText compat)
    pure ()

  getSubjectCompatibility (SubjectName subject) = do
    result <- runStatement getSubjectConfigStmt subject
    case result of
      Right (Just compat) -> pure $ Just $ parseCompatibility compat
      _ -> pure Nothing

  setSubjectCompatibility (SubjectName subject) compat = do
    _ <- runStatement setSubjectConfigStmt (subject, compatToText compat)
    pure ()

  deleteSubjectCompatibility (SubjectName subject) = do
    _ <- runStatement deleteSubjectConfigStmt subject
    pure ()

  -- Mode operations would follow similar pattern
  getGlobalMode = pure "READWRITE"
  setGlobalMode _ = pure ()
  getSubjectMode _ = pure Nothing
  setSubjectMode _ _ = pure ()
  deleteSubjectMode _ = pure ()

-- Helper functions
parseCompatibility :: Text -> CompatibilityLevel
parseCompatibility = \case
  "BACKWARD" -> BACKWARD
  "BACKWARD_TRANSITIVE" -> BACKWARD_TRANSITIVE
  "FORWARD" -> FORWARD
  "FORWARD_TRANSITIVE" -> FORWARD_TRANSITIVE
  "FULL" -> FULL
  "FULL_TRANSITIVE" -> FULL_TRANSITIVE
  "NONE" -> NONE
  _ -> BACKWARD

compatToText :: CompatibilityLevel -> Text
compatToText = \case
  BACKWARD -> "BACKWARD"
  BACKWARD_TRANSITIVE -> "BACKWARD_TRANSITIVE"
  FORWARD -> "FORWARD"
  FORWARD_TRANSITIVE -> "FORWARD_TRANSITIVE"
  FULL -> "FULL"
  FULL_TRANSITIVE -> "FULL_TRANSITIVE"
  NONE -> "NONE"

-- Schema for PostgreSQL tables
{-
CREATE TABLE schemas (
  id SERIAL PRIMARY KEY,
  schema TEXT NOT NULL,
  schema_type TEXT,
  hash TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE subject_versions (
  subject TEXT NOT NULL,
  version INT NOT NULL,
  schema_id INT NOT NULL REFERENCES schemas(id),
  deleted BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  PRIMARY KEY (subject, version)
);

CREATE INDEX idx_subject_versions_schema_id ON subject_versions(schema_id);
CREATE INDEX idx_subject_versions_deleted ON subject_versions(subject, deleted) WHERE NOT deleted;

CREATE TABLE configs (
  subject TEXT,
  compatibility TEXT NOT NULL,
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE (subject)
);

CREATE TABLE modes (
  subject TEXT,
  mode TEXT NOT NULL,
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE (subject)
);
-}
