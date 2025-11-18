{-# LANGUAGE QuasiQuotes #-}
module Fractal.Schema.Backend.PostgreSQL where
import Fractal.Schema.Backend.Class as Class
import Fractal.Schema.Types as Types
import Fractal.Schema.Compatibility.Avro as Avro
import Fractal.Schema.Compatibility.Json as Json
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified Hasql.TH as HTH
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Int (Int64)
import Data.Avro (Schema(..))
import qualified Data.Avro.Schema.Schema as AvroSchema
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Aeson (FromJSON(..), eitherDecode, encode, eitherDecodeStrictText)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)

-- Hasql-based store monad
newtype HasqlStore a = HasqlStore (ReaderT HC.Connection IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader HC.Connection)

runHasqlStore :: MonadIO m => HC.Connection -> HasqlStore a -> m a
runHasqlStore conn (HasqlStore m) = liftIO $ runReaderT m conn

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
  ON CONFLICT (subject) DO UPDATE SET compatibility = EXCLUDED.compatibility
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

getSchemaVersionsStmt :: HST.Statement Int64 (V.Vector (Text, Int64))
getSchemaVersionsStmt = [HTH.vectorStatement|
  SELECT subject :: text, version :: int8
  FROM subject_versions
  WHERE schema_id = $1 :: int8 AND NOT deleted
  ORDER BY subject, version
  |]

getReferencedByStmt :: HST.Statement (Text, Int64) (V.Vector (Text, Int64, Maybe Text, Text))
getReferencedByStmt = [HTH.vectorStatement|
  WITH referenced_schema AS (
    SELECT schema_id
    FROM subject_versions
    WHERE subject = $1 :: text AND version = $2 :: int8 AND NOT deleted
  )
  SELECT
    sv.subject :: text,
    sv.version :: int8,
    s.schema_type :: text?,
    s.schema :: text
  FROM subject_versions sv
  JOIN schemas s ON sv.schema_id = s.id
  WHERE sv.schema_id IN (SELECT schema_id FROM referenced_schema)
    AND sv.deleted = false
  ORDER BY sv.subject, sv.version
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
      Left err ->
        let errMsg = show err in
        if "duplicate key value violates unique constraint" `T.isInfixOf` T.pack errMsg && "schemas_hash_key" `T.isInfixOf` T.pack errMsg
          then do
            liftIO $ putStrLn $ "Schema with hash already exists, returning existing ID. Hash: " ++ T.unpack hash
            mExisting <- runStatement findSchemaByHashStmt hash
            case mExisting of
              Right (Just record) -> pure $ Right $ Class.srId record
              _ -> do
                liftIO $ putStrLn $ "Failed to find existing schema after unique constraint violation. Hash: " ++ T.unpack hash
                pure $ Left $ DatabaseError $ T.pack $ "Failed to insert schema (duplicate hash) and could not find existing: " ++ errMsg
          else do
            liftIO $ putStrLn $ "Failed to insert schema: " ++ errMsg ++ "\nSchema type: " ++ show schemaType ++ "\nHash: " ++ T.unpack hash
            pure $ Left $ DatabaseError $ T.pack $ "Failed to insert schema: " ++ errMsg

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
      Left err -> pure $ Left $ DatabaseError $ T.pack $ "Failed to register schema version: " ++ show err

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
      _ -> do
        -- Set default to FULL if not configured
        _ <- runStatement setGlobalConfigStmt "FULL"
        pure FULL

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

  getSchemaVersions (SchemaId sid) = do
    result <- runStatement getSchemaVersionsStmt sid
    case result of
      Right versions -> pure $ Right $ fmap (\(subject, version) -> SubjectVersion subject (Version version)) versions
      Left err -> pure $ Left err

  getReferencedBy (SubjectName subject) (Version version) = do
    result <- runStatement getReferencedByStmt (subject, version)
    case result of
      Right refs -> pure $ Right $ fmap (\(subj, ver, schemaType, schema) ->
        SchemaInfo
          subj
          (SchemaId 0) -- We don't have the schema ID in the result
          (Version ver)
          (case schemaType of
            Just "AVRO" -> Just AVRO
            Just "JSON" -> Just JSON
            Just "PROTOBUF" -> Just PROTOBUF
            _ -> Nothing)
          schema) refs
      Left err -> pure $ Left err

  checkCompatibility (SubjectName subject) (Version version) newSchema = do
    -- Get the existing schema
    versionResult <- Class.getSubjectVersion (SubjectName subject) (Version version)
    case versionResult of
      Right (schemaId, _) -> do
        schemaResult <- getSchema schemaId
        case schemaResult of
          Right existingSchema -> do
            -- Get compatibility level
            subjectCompat <- getSubjectCompatibility (SubjectName subject)
            globalCompat <- getGlobalCompatibility
            let level = fromMaybe globalCompat subjectCompat

            -- Check compatibility based on schema type
            case (existingSchema.srSchemaType, newSchema.schemaType) of
              (Just AVRO, Just AVRO) -> do
                -- Parse both schemas
                case (eitherDecodeStrictText existingSchema.srSchema,
                      eitherDecodeStrictText newSchema.schema) of
                  (Right existingAvro, Right newAvro) -> do
                    -- Create schema environment for references
                    let env = Avro.SchemaEnv mempty mempty -- TODO: Add support for references
                    -- Check compatibility based on level
                    let result = case level of
                          NONE -> Avro.Compatible
                          BACKWARD -> Avro.checkBackwardCompatibility existingAvro newAvro
                          FORWARD -> Avro.checkForwardCompatibility existingAvro newAvro
                          FULL -> Avro.checkFullCompatibility existingAvro newAvro
                          BACKWARD_TRANSITIVE -> Avro.checkBackwardTransitive (newAvro :| [existingAvro])
                          FORWARD_TRANSITIVE -> Avro.checkForwardTransitive (newAvro :| [existingAvro])
                          FULL_TRANSITIVE -> Avro.checkFullTransitive (newAvro :| [existingAvro])
                    case result of
                      Avro.Compatible -> pure $ Right $ Types.CompatibilityCheckResult True Nothing
                      Avro.Incompatible errors -> pure $ Right $ Types.CompatibilityCheckResult False $ Just $ fmap convertError $ toList errors
                  (Left err, _) -> pure $ Left $ DatabaseError $ T.pack $ "Failed to parse existing schema: " ++ show err
                  (_, Left err) -> pure $ Left $ DatabaseError $ T.pack $ "Failed to parse new schema: " ++ show err
              (Just JSON, Just JSON) -> do
                -- Parse both JSON schemas
                case (eitherDecodeStrictText existingSchema.srSchema,
                      eitherDecodeStrictText newSchema.schema) of
                  (Right existingJson, Right newJson) -> do
                    -- Check compatibility based on level
                    let result = case level of
                          NONE -> Json.Compatible
                          BACKWARD -> Json.checkBackwardCompatibility existingJson newJson
                          FORWARD -> Json.checkForwardCompatibility existingJson newJson
                          FULL -> Json.checkFullCompatibility existingJson newJson
                          BACKWARD_TRANSITIVE -> Json.checkBackwardTransitive (newJson :| [existingJson])
                          FORWARD_TRANSITIVE -> Json.checkForwardTransitive (newJson :| [existingJson])
                          FULL_TRANSITIVE -> Json.checkFullTransitive (newJson :| [existingJson])
                    case result of
                      Json.Compatible -> pure $ Right $ Types.CompatibilityCheckResult True Nothing
                      Json.Incompatible errors -> pure $ Right $ Types.CompatibilityCheckResult False $ Just $ fmap convertJsonError $ toList errors
                  (Left err, _) -> pure $ Left $ DatabaseError $ T.pack $ "Failed to parse existing JSON schema: " ++ show err
                  (_, Left err) -> pure $ Left $ DatabaseError $ T.pack $ "Failed to parse new JSON schema: " ++ show err
              (Just PROTOBUF, Just PROTOBUF) -> do
                -- TODO: Implement Protobuf compatibility check
                pure $ Right $ Types.CompatibilityCheckResult True Nothing
              _ -> pure $ Right $ Types.CompatibilityCheckResult False $ Just [Types.CompatibilityError Types.TypeMismatch "" "Schema types do not match"]
          Left err -> pure $ Left err
      Left err -> pure $ Left err

  checkCompatibilityLatest (SubjectName subject) newSchema = do
    -- Get the latest version
    latestResult <- Class.getLatestVersion (SubjectName subject)
    case latestResult of
      Right version -> Class.checkCompatibility (SubjectName subject) version newSchema
      Left err -> pure $ Left err

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

convertError :: Avro.CompatibilityError -> Types.CompatibilityError
convertError err = Types.CompatibilityError
  { error_type = convertErrorType $ Avro.errorType err
  , error_path = Avro.formatPath $ Avro.errorPath err
  , error_details = Avro.errorDetails err
  }

convertErrorType :: Avro.ErrorType -> Types.ErrorType
convertErrorType = \case
  Avro.TypeMismatch -> Types.TypeMismatch
  Avro.MissingField -> Types.MissingField
  Avro.MissingDefault -> Types.MissingDefault
  Avro.NameMismatch -> Types.NameMismatch
  Avro.SizeMismatch -> Types.SizeMismatch
  Avro.EnumSymbolMismatch -> Types.EnumSymbolMismatch
  Avro.UnionVariantMismatch -> Types.UnionVariantMismatch
  Avro.PromotionError _ _ -> Types.PromotionError
  Avro.ReferenceError -> Types.ReferenceError
  Avro.AliasError -> Types.AliasError

convertJsonError :: Json.CompatibilityError -> Types.CompatibilityError
convertJsonError err = Types.CompatibilityError
  { error_type = convertJsonErrorType $ Json.errorType err
  , error_path = Json.formatPath $ Json.errorPath err
  , error_details = Json.errorDetails err
  }

convertJsonErrorType :: Json.ErrorType -> Types.ErrorType
convertJsonErrorType = \case
  Json.TypeMismatch -> Types.TypeMismatch
  Json.RequiredFieldAdded -> Types.MissingField
  Json.RequiredFieldRemoved -> Types.MissingField
  Json.EnumValueRemoved -> Types.EnumSymbolMismatch
  Json.EnumValueAdded -> Types.EnumSymbolMismatch
  Json.ConstraintTightened -> Types.TypeMismatch
  Json.ConstraintRelaxed -> Types.TypeMismatch
  Json.PropertyRemoved -> Types.MissingField
  Json.PropertyAdded -> Types.MissingField
  Json.TypeNarrowed -> Types.TypeMismatch
  Json.TypeWidened -> Types.TypeMismatch
  Json.FormatChanged -> Types.TypeMismatch

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
