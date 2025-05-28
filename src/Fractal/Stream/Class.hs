{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Fractal.Stream.Class where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

import Fractal.Stream.Types

-- Core streaming capabilities
class MonadIO m => MonadStream m where
  type StreamBackend m

  -- Publishing
  publish :: ToJSON e {- TODO, get rid of this constriant -} => (StreamBackend m) -> EventEnvelope e -> PublishConfig -> m (Either StreamError EventId)

  -- Consumer management
  createConsumer :: StreamBackend m -> StreamConfig -> m (Either StreamError ConsumerState)
  subscribe :: StreamBackend m -> ConsumerState -> (EventEnvelope ByteString -> m (ProcessingResult ())) -> m ()

  -- Offset management
  commitOffset :: StreamBackend m -> ConsumerGroupId -> StreamOffset -> m (Either StreamError ())
  getOffset :: StreamBackend m -> ConsumerGroupId -> m (Either StreamError StreamOffset)
  resetOffset :: StreamBackend m -> ConsumerGroupId -> StartOffset -> m (Either StreamError ())

  -- Stream operations
  replayEvents :: StreamBackend m -> ReplayConfig -> m (Either StreamError ())
  getStreamInfo :: StreamBackend m -> Text -> m (Either StreamError StreamInfo)

-- Stream information
data StreamInfo = StreamInfo
  { streamName :: Text
  , streamLength :: Integer
  , streamFirstEntry :: Maybe (StreamOffset, UTCTime)
  , streamLastEntry :: Maybe (StreamOffset, UTCTime)
  , streamConsumerGroups :: [ConsumerGroupId]
  } deriving (Eq, Show)

-- Helper constraint for stream processing
type StreamProcess m = (MonadStream m, MonadIO m)
