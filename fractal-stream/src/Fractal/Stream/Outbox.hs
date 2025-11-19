{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Fractal.Stream.Outbox
  ( -- * Outbox Types
    OutboxEvent(..)
  , OutboxProcessor(..)
  , OutboxConfig(..)

  -- * Outbox Operations
  , writeToOutbox
  , startOutboxProcessor
  , processOutboxBatch

  -- * Database Operations
  , createOutboxTable
  , getUnprocessedEvents
  , markAsProcessed
  , updateOutboxEvent

  -- * Hasql Sessions
  , outboxSession
  ) where

import Contravariant.Extras
import Control.Monad (forever, when, void)
import Data.Aeson (ToJSON(..), Value, encode, decode, eitherDecode', object, (.=))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor.Contravariant
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID, nil)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Hasql.Connection (Connection)
import Hasql.Session (Session, QueryError)
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.Statement as Statement
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Transaction (Transaction, statement)
import qualified Hasql.Transaction.Sessions as TransactionSessions
import UnliftIO
import UnliftIO.Concurrent

import Fractal.Stream.Types
import Fractal.Stream.Class

-- Outbox event structure
data OutboxEvent = OutboxEvent
  { outboxEventId :: UUID
  , outboxEventTopic :: Text
  , outboxEventKey :: Maybe Text
  , outboxEventPayload :: ByteString
  , outboxEventMetadata :: Value
  , outboxEventCreatedAt :: UTCTime
  , outboxEventProcessedAt :: Maybe UTCTime
  , outboxEventRetries :: Int32
  , outboxEventError :: Maybe Text
  } deriving (Eq, Show, Generic)

-- Outbox processor configuration
data OutboxConfig = OutboxConfig
  { outboxBatchSize :: Int32
  , outboxPollInterval :: Int  -- milliseconds
  , outboxMaxRetries :: Int32
  , outboxRetryDelay :: Int    -- milliseconds
  , outboxConnection :: Connection
  }

-- Outbox processor handle
data OutboxProcessor = OutboxProcessor
  { processorConfig :: OutboxConfig
  , processorActive :: TVar Bool
  }

-- SQL Statements using Hasql

-- Create outbox table
createOutboxTableStatement :: Statement () ()
createOutboxTableStatement = Statement.Statement sql Encoders.noParams Decoders.noResult True
  where
    sql = "CREATE TABLE IF NOT EXISTS outbox_events ( \
          \  event_id UUID PRIMARY KEY, \
          \  topic TEXT NOT NULL, \
          \  key TEXT, \
          \  payload BYTEA NOT NULL, \
          \  metadata JSONB NOT NULL DEFAULT '{}', \
          \  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(), \
          \  processed_at TIMESTAMPTZ, \
          \  retries INT NOT NULL DEFAULT 0, \
          \  error TEXT \
          \)"

-- Create unprocessed events index
createUnprocessedIndexStatement :: Statement () ()
createUnprocessedIndexStatement = Statement.Statement sql Encoders.noParams Decoders.noResult True
  where
    sql = "CREATE INDEX IF NOT EXISTS idx_unprocessed ON outbox_events (processed_at) WHERE processed_at IS NULL"

-- Create created_at index
createCreatedIndexStatement :: Statement () ()
createCreatedIndexStatement = Statement.Statement sql Encoders.noParams Decoders.noResult True
  where
    sql = "CREATE INDEX IF NOT EXISTS idx_created ON outbox_events (created_at)"

-- Insert outbox event
insertOutboxEventStatement :: Statement OutboxEvent ()
insertOutboxEventStatement = Statement.Statement sql encoder Decoders.noResult True
  where
    sql = "INSERT INTO outbox_events \
          \  (event_id, topic, key, payload, metadata, created_at, retries) \
          \VALUES ($1, $2, $3, $4, $5, $6, $7)"

    encoder =
      (outboxEventId >$< Encoders.param (Encoders.nonNullable Encoders.uuid)) <>
      (outboxEventTopic >$< Encoders.param (Encoders.nonNullable Encoders.text)) <>
      (outboxEventKey >$< Encoders.param (Encoders.nullable Encoders.text)) <>
      (outboxEventPayload >$< Encoders.param (Encoders.nonNullable Encoders.bytea)) <>
      (outboxEventMetadata >$< Encoders.param (Encoders.nonNullable Encoders.jsonb)) <>
      (outboxEventCreatedAt >$< Encoders.param (Encoders.nonNullable Encoders.timestamptz)) <>
      (outboxEventRetries >$< Encoders.param (Encoders.nonNullable Encoders.int4))

-- Get unprocessed events
getUnprocessedEventsStatement :: Statement Int32 (Vector OutboxEvent)
getUnprocessedEventsStatement = Statement.Statement sql encoder decoder True
  where
    sql = "SELECT event_id, topic, key, payload, metadata, created_at, processed_at, retries, error \
          \FROM outbox_events \
          \WHERE processed_at IS NULL \
          \  AND (retries < 3 OR error IS NULL) \
          \ORDER BY created_at ASC \
          \LIMIT $1 \
          \FOR UPDATE SKIP LOCKED"

    encoder = Encoders.param $ Encoders.nonNullable Encoders.int4

    decoder :: Decoders.Result (Vector OutboxEvent)
    decoder = Decoders.rowVector outboxEventDecoder

    outboxEventDecoder = OutboxEvent
      <$> Decoders.column (Decoders.nonNullable Decoders.uuid)
      <*> Decoders.column (Decoders.nonNullable Decoders.text)
      <*> Decoders.column (Decoders.nullable Decoders.text)
      <*> Decoders.column (Decoders.nonNullable Decoders.bytea)
      <*> Decoders.column (Decoders.nonNullable $ Decoders.jsonbBytes (first T.pack . eitherDecode' . LBS.fromStrict))
      <*> Decoders.column (Decoders.nonNullable Decoders.timestamptz)
      <*> Decoders.column (Decoders.nullable Decoders.timestamptz)
      <*> Decoders.column (Decoders.nonNullable Decoders.int4)
      <*> Decoders.column (Decoders.nullable Decoders.text)

-- Mark event as processed
markAsProcessedStatement :: Statement (UUID, UTCTime) ()
markAsProcessedStatement = Statement.Statement sql encoder Decoders.noResult True
  where
    sql = "UPDATE outbox_events \
          \SET processed_at = $2 \
          \WHERE event_id = $1"

    encoder = contrazip2
      (Encoders.param $ Encoders.nonNullable Encoders.uuid)
      (Encoders.param $ Encoders.nonNullable Encoders.timestamptz)

-- Update event for retry
updateRetryStatement :: Statement (UUID, Int32, Text) ()
updateRetryStatement = Statement.Statement sql encoder Decoders.noResult True
  where
    sql = "UPDATE outbox_events \
          \SET retries = $2, error = $3 \
          \WHERE event_id = $1"

    encoder :: Encoders.Params (UUID, Int32, Text)
    encoder = contrazip3
      (Encoders.param $ Encoders.nonNullable Encoders.uuid)
      (Encoders.param $ Encoders.nonNullable Encoders.int4)
      (Encoders.param $ Encoders.nonNullable Encoders.text)

-- Database operations
createOutboxTable :: MonadIO m => Connection -> m (Either QueryError ())
createOutboxTable conn = liftIO $ do
  let transaction = do
        statement () createOutboxTableStatement
        statement () createUnprocessedIndexStatement
        statement () createCreatedIndexStatement
  Session.run (TransactionSessions.transaction TransactionSessions.ReadCommitted TransactionSessions.Write transaction) conn

writeToOutbox :: (MonadIO m, ToJSON e) =>
                 Connection -> Text -> Maybe Text -> EventEnvelope e -> m (Either QueryError ())
writeToOutbox conn topic key envelope = liftIO $ do
  now <- getCurrentTime
  let outboxEvent = OutboxEvent
        { outboxEventId = case _eventId envelope of EventId uuid -> uuid
        , outboxEventTopic = topic
        , outboxEventKey = key
        , outboxEventPayload = LBS.toStrict $ encode envelope
        , outboxEventMetadata = toJSON $ _metadata envelope
        , outboxEventCreatedAt = now
        , outboxEventProcessedAt = Nothing
        , outboxEventRetries = 0
        , outboxEventError = Nothing
        }

  let transaction = statement outboxEvent insertOutboxEventStatement
  Session.run (TransactionSessions.transaction TransactionSessions.ReadCommitted TransactionSessions.Write transaction) conn

getUnprocessedEvents :: MonadIO m => Connection -> Int32 -> m (Either QueryError (Vector OutboxEvent))
getUnprocessedEvents conn limit = liftIO $ do
  let transaction = statement limit getUnprocessedEventsStatement
  Session.run (TransactionSessions.transaction TransactionSessions.ReadCommitted TransactionSessions.Write transaction) conn

markAsProcessed :: MonadIO m => Connection -> UUID -> m (Either QueryError ())
markAsProcessed conn eid = liftIO $ do
  now <- getCurrentTime
  let transaction = statement (eid, now) markAsProcessedStatement
  Session.run (TransactionSessions.transaction TransactionSessions.ReadCommitted TransactionSessions.Write transaction) conn

updateOutboxEvent :: MonadIO m => Connection -> UUID -> Int32 -> Text -> m (Either QueryError ())
updateOutboxEvent conn eid retries errorMsg = liftIO $ do
  let transaction = statement (eid, retries, errorMsg) updateRetryStatement
  Session.run (TransactionSessions.transaction TransactionSessions.ReadCommitted TransactionSessions.Write transaction) conn

-- Outbox processor implementation
startOutboxProcessor :: (MonadStream m, MonadUnliftIO m) =>
                        StreamBackend m -> OutboxConfig -> m OutboxProcessor
startOutboxProcessor backend config = do
  activeVar <- newTVarIO True
  let processor = OutboxProcessor config activeVar

  -- Fork processor thread
  void $ forkIO $ forever $ do
    active <- readTVarIO activeVar
    when active $ do
      -- Process batch
      processOutboxBatch backend config

      -- Wait before next poll
      threadDelay $ outboxPollInterval config * 1000

  return processor

-- Process a batch of outbox events
processOutboxBatch :: (MonadStream m) =>
                      StreamBackend m -> OutboxConfig -> m ()
processOutboxBatch backend config = do
  result <- getUnprocessedEvents (outboxConnection config) (outboxBatchSize config)
  case result of
    Left err -> liftIO $ putStrLn $ "Failed to get unprocessed events: " <> show err
    Right events -> do
      -- Process each event
      V.forM_ events $ \event -> do
        publishResult <- publishOutboxEvent backend event
        case publishResult of
          Right _ -> do
            -- Mark as processed
            void $ markAsProcessed (outboxConnection config) (outboxEventId event)
          Left streamErr -> do
            -- Handle retry logic
            handleRetry config event streamErr

-- Publish outbox event to stream
publishOutboxEvent :: (MonadStream m) =>
                      StreamBackend m -> OutboxEvent -> m (Either StreamError EventId)
publishOutboxEvent backend outboxEvent = do
  -- Deserialize envelope
  case decode (LBS.fromStrict $ outboxEventPayload outboxEvent) of
    Nothing -> return $ Left $ DeserializationError "Failed to decode event envelope"
    Just (envelope :: EventEnvelope Value) -> do
      -- Publish to stream
      let config = PublishConfig
            { _publishTopic = outboxEventTopic outboxEvent
            , _publishKey = outboxEventKey outboxEvent
            , _publishHeaders = []
            }
      publish backend envelope config

-- Handle retry logic
handleRetry :: MonadIO m => OutboxConfig -> OutboxEvent -> StreamError -> m ()
handleRetry config event err = do
  let retries = outboxEventRetries event + 1
  if retries < outboxMaxRetries config
    then do
      -- Update retry count and error
      void $ updateOutboxEvent (outboxConnection config)
                               (outboxEventId event)
                               retries
                               (T.pack $ show err)
    else do
      -- Log permanent failure
      liftIO $ putStrLn $ "Event permanently failed after " <> show retries <> " retries: " <> show (outboxEventId event)

-- Session helper for transactions
outboxSession :: Connection -> Session a -> IO (Either QueryError a)
outboxSession = flip Session.run
