{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestDatabase
  ( withTestDatabase
  , setupDatabaseSchema
  ) where

import qualified Database.Postgres.Temp as TmpPostgres
import qualified Hasql.Connection as HC
import qualified Hasql.Session as HS
import qualified Hasql.Statement as HST
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)

-- | Start a temporary PostgreSQL server and run an action with a connection
withTestDatabase :: (HC.Connection -> IO a) -> IO a
withTestDatabase action = do
  -- Start a temporary PostgreSQL instance
  -- This will download PostgreSQL if needed (cached for future runs)
  -- and start it on a random available port
  TmpPostgres.withDbCache $ \dbCache -> do
    result <- TmpPostgres.withConfig (TmpPostgres.cacheConfig dbCache) $ \db -> do
      -- Get the connection string from the temporary database
      let connString = TmpPostgres.toConnectionString db

      -- Acquire connection
      bracket (HC.acquire connString)
              (\conn -> case conn of
                  Right c -> HC.release c
                  Left _ -> pure ())
              $ \conn -> case conn of
                  Right c -> do
                    -- Set up the database schema
                    setupDatabaseSchema c
                    -- Run the action
                    action c
                  Left err -> error $ "Failed to connect to test database: " ++ show err

    case result of
      Right r -> pure r
      Left err -> error $ "Failed to start test database: " ++ show err

-- | Set up the database schema for tests
setupDatabaseSchema :: HC.Connection -> IO ()
setupDatabaseSchema conn = do
  -- Create schemas table
  let createSchemas :: HST.Statement () ()
      createSchemas = HST.Statement sql encoder decoder True
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

  result1 <- HS.run (HS.statement () createSchemas) conn
  case result1 of
    Left err -> error $ "Failed to create schemas table: " ++ show err
    Right _ -> pure ()

  -- Create subject_versions table
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

  -- Create indexes
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

  -- Create configs table
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

  -- Create modes table
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

  result6 <- HS.run (HS.statement () createModes) conn
  case result6 of
    Left err -> error $ "Failed to create modes table: " ++ show err
    Right _ -> pure ()

  pure ()
