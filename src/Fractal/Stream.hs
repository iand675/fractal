{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fractal.Stream
  ( -- * Core Types
    EventEnvelope(..)
  , EventId(..)
  , SchemaVersion(..)
  , ServiceIdentifier(..)
  , ConsumerGroupId(..)
  , StreamOffset(..)
  , TraceContext(..)
  , EventMetadata(..)
  , StreamConfig(..)
  , StartOffset(..)
  , PublishConfig(..)
  , StreamError(..)
  , ProcessingResult(..)
  , ReplayConfig(..)
  , EventFilter(..)

  -- * Stream Operations
  , Fractal.Stream.publish
  , publishEvent
  , consumer
  , Fractal.Stream.subscribe
  , subscribeForever
  , replayEvents
  , resetOffset

  -- * Builders
  , mkEventEnvelope
  , mkPublishConfig
  , mkStreamConfig
  , mkReplayConfig

  -- * Re-exports
  , MonadStream
  , StreamProcess
  ) where

import Control.Lens
import Control.Monad (forever, when)
import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID, toText)
import qualified Data.UUID.V4 as UUID
import Validation

import Fractal.Stream.Types
import Fractal.Stream.Class
import UnliftIO
import UnliftIO.Concurrent

-- High-level publish function
publish :: (MonadStream m, ToJSON e) =>
           StreamBackend m -> PublishConfig -> EventEnvelope e -> m EventId
publish backend config event = do
  result <- Fractal.Stream.Class.publish backend event config
  case result of
    Left err -> liftIO $ throwIO $ StreamException err
    Right eventId -> return eventId

-- Simplified event publishing
publishEvent :: (MonadStream m, ToJSON e) =>
                StreamBackend m -> Text -> Text -> e -> m EventId
publishEvent backend topic eventType payload = do
  event <- mkEventEnvelope eventType payload
  let config = mkPublishConfig topic
  Fractal.Stream.publish backend config event

-- Create a consumer
consumer :: MonadStream m =>
            StreamBackend m -> StreamConfig -> m ConsumerState
consumer backend config = do
  result <- createConsumer backend config
  case result of
    Left err -> liftIO $ throwIO $ StreamException err
    Right state -> return state

-- Subscribe to events with automatic deserialization
subscribe :: forall m e. (MonadStream m, FromJSON e) =>
             StreamBackend m -> ConsumerState -> (EventEnvelope e -> m (ProcessingResult ())) -> m ()
subscribe backend state handler = do
  Fractal.Stream.Class.subscribe backend state $ \envelope -> do
    -- Deserialize payload
    case decodePayload envelope of
      Left err -> return $ failure $ DeserializationError err
      Right typedEnvelope -> handler typedEnvelope
  where
    decodePayload :: FromJSON e => EventEnvelope ByteString -> Either Text (EventEnvelope e)
    decodePayload env = case eitherDecode' (LBS.fromStrict $ _payload env) of
      Left err -> Left $ T.pack err
      Right payload -> Right $ env { _payload = payload }

    eitherDecode' = undefined -- Would use Data.Aeson.eitherDecode

-- Subscribe forever with error handling
subscribeForever :: forall m e. (MonadStream m, MonadUnliftIO m, FromJSON e) =>
                    StreamBackend m -> ConsumerState -> (EventEnvelope e -> m (ProcessingResult ())) -> m ()
subscribeForever backend state handler = forever $ do
  Fractal.Stream.subscribe backend state handler `catch` handleError
  where
    handleError :: StreamException -> m ()
    handleError (StreamException err) = do
      -- Log error
      liftIO $ putStrLn $ "Stream error: " <> show err
      -- Wait before retrying
      liftIO $ threadDelay 5000000  -- 5 seconds

-- Event envelope builder
mkEventEnvelope :: (MonadIO m, ToJSON e) => Text -> e -> m (EventEnvelope e)
mkEventEnvelope eventType payload = liftIO $ do
  eventId <- EventId <$> UUID.nextRandom
  eventTime <- getCurrentTime
  traceId <- toText <$> UUID.nextRandom
  spanId <- toText <$> UUID.nextRandom

  return EventEnvelope
    { _eventId = eventId
    , _eventType = eventType
    , _eventVersion = SchemaVersion 1
    , _eventTime = eventTime
    , _eventSource = ServiceIdentifier "fractal-stream"  -- Would be configurable
    , _traceContext = TraceContext traceId spanId Nothing Nothing
    , _payload = payload
    , _metadata = EventMetadata mempty
    }

-- Config builders
mkPublishConfig :: Text -> PublishConfig
mkPublishConfig topic = PublishConfig
  { _publishTopic = topic
  , _publishKey = Nothing
  , _publishHeaders = []
  }

mkStreamConfig :: [Text] -> ConsumerGroupId -> StreamConfig
mkStreamConfig topics groupId = StreamConfig
  { _streamTopics = topics
  , _streamGroupId = groupId
  , _streamBatchSize = 100
  , _streamMaxPollInterval = 5000
  , _streamStartOffset = FromEnd
  }

mkReplayConfig :: UTCTime -> ConsumerGroupId -> ReplayConfig
mkReplayConfig startTime targetGroupId = ReplayConfig
  { _replayStartTime = startTime
  , _replayEndTime = Nothing
  , _replayFilters = []
  , _replayParallelism = 1
  , _replayTargetGroupId = targetGroupId
  }

-- Exception wrapper
newtype StreamException = StreamException StreamError
  deriving (Show)

instance Exception StreamException
