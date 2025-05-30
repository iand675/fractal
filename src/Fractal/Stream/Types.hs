{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Fractal.Stream.Types where

import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Validation

-- Core identifiers
newtype EventId = EventId UUID
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype SchemaVersion = SchemaVersion Int
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype ServiceIdentifier = ServiceIdentifier Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype ConsumerGroupId = ConsumerGroupId Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype StreamOffset = StreamOffset Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- Event metadata for routing and filtering
newtype EventMetadata = EventMetadata (Map Text Text)
  deriving newtype (Eq, Show, ToJSON, FromJSON, Semigroup, Monoid)

-- Core event envelope structure
data EventEnvelope e = EventEnvelope
  { _eventId :: EventId
  , _eventType :: Text
  , _eventVersion :: SchemaVersion
  , _eventTime :: UTCTime
  , _eventSource :: ServiceIdentifier
  , _payload :: e
  , _metadata :: EventMetadata
  } deriving (Eq, Show, Generic)

instance ToJSON e => ToJSON (EventEnvelope e)
instance FromJSON e => FromJSON (EventEnvelope e)

-- Stream configuration
data StreamConfig = StreamConfig
  { _streamTopics :: [Text]
  , _streamGroupId :: ConsumerGroupId
  , _streamBatchSize :: Int
  , _streamMaxPollInterval :: Int  -- milliseconds
  , _streamStartOffset :: StartOffset
  } deriving (Eq, Show)

data StartOffset
  = FromBeginning
  | FromEnd
  | FromOffset StreamOffset
  | FromTimestamp UTCTime
  deriving (Eq, Show)

-- Publishing configuration
data PublishConfig = PublishConfig
  { _publishTopic :: Text
  , _publishKey :: Maybe Text
  , _publishHeaders :: [(Text, Text)]
  } deriving (Eq, Show)

-- Error types
data StreamError
  = ConnectionError Text
  | SerializationError Text
  | DeserializationError Text
  | PublishError Text
  | ConsumerError Text
  | OffsetError Text
  | SchemaError Text
  deriving (Eq, Show)

-- Offset storage
data OffsetStore
  = RedisOffsetStore
  | DbOffsetStore -- Placeholder for database-backed storage
  | MemoryOffsetStore
  deriving (Eq, Show)

-- Consumer state
data ConsumerState = ConsumerState
  { _consumerConfig :: StreamConfig
  , _consumerOffset :: StreamOffset
  , _consumerActive :: Bool
  } deriving (Eq, Show)

-- Replay configuration
data ReplayConfig = ReplayConfig
  { _replayStartTime :: UTCTime
  , _replayEndTime :: Maybe UTCTime
  , _replayFilters :: [EventFilter]
  , _replayParallelism :: Int
  , _replayTargetGroupId :: ConsumerGroupId
  } deriving (Eq, Show)

data EventFilter = EventFilter
  { _filterField :: Text
  , _filterValue :: Text
  } deriving (Eq, Show, Generic)

-- Processing result for railway-oriented programming
-- Using Validation from validation-selective for better error accumulation
type ProcessingResult a = Validation (NonEmpty StreamError) a
