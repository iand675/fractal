{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Fractal.Stream.Backend.Redis where

import Control.Monad (forever, forM, forM_, when, void)
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.UUID as UUID
import Database.Redis as Redis
import Validation
import UnliftIO
import UnliftIO.Concurrent

import Fractal.Stream.Types
import Fractal.Stream.Class

-- Redis-specific configuration
data RedisStreamEnv = RedisStreamEnv
  { redisConnection :: Connection
  , redisConsumerMap :: TVar [(ConsumerGroupId, TVar Bool)]  -- Track active consumers
  }

-- Helper to create Redis stream env
createRedisStreamEnv :: ConnectInfo -> IO RedisStreamEnv
createRedisStreamEnv connInfo = do
  conn <- connect connInfo
  consumerMap <- newTVarIO []
  return $ RedisStreamEnv conn consumerMap

-- Redis stream key helpers
streamKey :: Text -> ByteString
streamKey topic = T.encodeUtf8 $ "stream:" <> topic

consumerGroupKey :: ConsumerGroupId -> ByteString
consumerGroupKey (ConsumerGroupId gid) = T.encodeUtf8 $ "consumer:group:" <> gid

offsetKey :: ConsumerGroupId -> ByteString
offsetKey (ConsumerGroupId gid) = T.encodeUtf8 $ "offset:" <> gid

-- Convert event to Redis stream fields
eventToRedisFields :: ToJSON e => EventEnvelope e -> [(ByteString, ByteString)]
eventToRedisFields EventEnvelope{..} =
  [ ("event_id", T.encodeUtf8 $ UUID.toText $ case _eventId of EventId uuid -> uuid)
  , ("event_type", T.encodeUtf8 _eventType)
  , ("event_version", BS8.pack $ show $ case _eventVersion of SchemaVersion v -> v)
  , ("event_time", T.encodeUtf8 $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" _eventTime)
  , ("event_source", T.encodeUtf8 $ case _eventSource of ServiceIdentifier s -> s)
  , ("payload", LBS.toStrict $ encode _payload)
  , ("metadata", LBS.toStrict $ encode _metadata)
  ]

-- Convert Redis fields back to event
redisFieldsToEvent :: [(ByteString, ByteString)] -> Either StreamError (EventEnvelope ByteString)
redisFieldsToEvent fields = do
  let lookupField name = lookup name fields

  eventId <- case lookupField "event_id" >>= (UUID.fromText . T.decodeUtf8) of
    Just uuid -> Right $ EventId uuid
    Nothing -> Left $ DeserializationError "Missing or invalid event_id"

  let eventType = maybe "" T.decodeUtf8 $ lookupField "event_type"

  eventVersion <- case lookupField "event_version" >>= (readMaybe . BS8.unpack) of
    Just v -> Right $ SchemaVersion v
    Nothing -> Left $ DeserializationError "Missing or invalid event_version"

  eventTime <- case lookupField "event_time" of
    Just t -> case reads (T.unpack $ T.decodeUtf8 t) of
      [(time, "")] -> Right time
      _ -> Left $ DeserializationError "Invalid event_time format"
    Nothing -> Left $ DeserializationError "Missing event_time"

  let eventSource = ServiceIdentifier $ maybe "" T.decodeUtf8 $ lookupField "event_source"

  let payload = fromMaybe "" $ lookupField "payload"

  metadata <- case lookupField "metadata" >>= (Data.Aeson.decode . LBS.fromStrict) of
    Just md -> Right md
    Nothing -> Right $ EventMetadata mempty

  Right $ EventEnvelope eventId eventType eventVersion eventTime eventSource payload metadata

-- MonadStream instance for Redis
instance MonadUnliftIO m => MonadStream (RedisStreamT m) where
  type StreamBackend (RedisStreamT m) = RedisStreamEnv

  publish :: forall e. ToJSON e => RedisStreamEnv -> EventEnvelope e -> PublishConfig -> RedisStreamT m (Either StreamError EventId)
  publish env event config = RedisStreamT $ liftIO $ do
    let topic = _publishTopic config
        streamName = streamKey topic
        fields = eventToRedisFields event

    result <- runRedis (redisConnection env) $ do
      -- Use * for auto-generated ID
      xadd streamName "*" fields

    case result of
      Left err -> do
        return $ Left $ PublishError $ "Redis error: " <> T.pack (show err)
      Right msgId -> do
        return $ Right $ _eventId event

  createConsumer :: RedisStreamEnv -> StreamConfig -> RedisStreamT m (Either StreamError ConsumerState)
  createConsumer env config = RedisStreamT $ liftIO $ do
    let groupId = _streamGroupId config
        groupName = consumerGroupKey groupId

    -- Create consumer groups for each topic
    _results <- forM (_streamTopics config) $ \topic -> do
      let streamName = streamKey topic
      runRedis (redisConnection env) $ do
        -- Try to create group, ignore if already exists
        void $ xgroupCreate streamName groupName "$"

    -- Initialize consumer state
    activeVar <- newTVarIO True
    atomically $ modifyTVar (redisConsumerMap env) ((groupId, activeVar) :)


    return $ Right $ ConsumerState config (StreamOffset "$") True

  subscribe :: RedisStreamEnv -> ConsumerState -> (EventEnvelope ByteString -> RedisStreamT m (ProcessingResult ())) -> RedisStreamT m ()
  subscribe env state handler = RedisStreamT $ do
    let config = _consumerConfig state
        groupId = _streamGroupId config
        groupName = consumerGroupKey groupId
        consumerName = T.encodeUtf8 $ "consumer-" <> (case groupId of ConsumerGroupId g -> g)

    -- Find active flag for this consumer
    consumers <- readTVarIO (redisConsumerMap env)
    case lookup groupId consumers of
      Just activeVar -> do
        void $ forkIO $ forever $ do
          active <- readTVarIO activeVar
          when active $ do
            -- Read from multiple streams
            let streamNames = map (\t -> (streamKey t, ">")) (_streamTopics config)

            result <- liftIO $ runRedis (redisConnection env) $ do
              xreadGroupOpts groupName consumerName streamNames $ XReadOpts (Just 1000) (Just $ fromIntegral $ _streamBatchSize config)

            case result of
              Right mReadings -> forM_ mReadings $ \readings -> do
                forM_ readings $ \(XReadResponse stream messages) -> do
                  forM_ messages $ \(StreamsRecord msgId fields) -> do
                    case redisFieldsToEvent fields of
                      Right event -> do
                        -- Process event
                        processingResult <- runRedisStreamT $ handler event
                        -- Acknowledge on success
                        validation
                          (\errs -> do
                            -- Log error but don't acknowledge
                            return ())
                          (\_ -> do
                            void $ liftIO $ runRedis (redisConnection env) $ do
                              xack stream groupName [msgId])
                          processingResult
                      Left err -> do
                        -- Acknowledge to prevent stuck messages
                        void $ liftIO $ runRedis (redisConnection env) $ do
                          xack stream groupName [msgId]
              Left err -> do
                threadDelay 1000000  -- 1 second
          threadDelay 10000  -- 10ms between polls

      Nothing -> do
        return ()

  commitOffset :: RedisStreamEnv -> ConsumerGroupId -> StreamOffset -> RedisStreamT m (Either StreamError ())
  commitOffset env groupId offset = RedisStreamT $ liftIO $ do
    let key = offsetKey groupId
        value = case offset of StreamOffset o -> T.encodeUtf8 o

    result <- runRedis (redisConnection env) $ do
      Redis.set key value

    case result of
      Right _ -> do
        return $ Right ()
      Left err -> do
        return $ Left $ OffsetError $ "Failed to commit offset: " <> T.pack (show err)

  getOffset :: RedisStreamEnv -> ConsumerGroupId -> RedisStreamT m (Either StreamError StreamOffset)
  getOffset env groupId = RedisStreamT $ liftIO $ do
    let key = offsetKey groupId

    result <- runRedis (redisConnection env) $ do
      get key

    case result of
      Right (Just value) -> return $ Right $ StreamOffset $ T.decodeUtf8 value
      Right Nothing -> return $ Right $ StreamOffset "$"  -- Default to end of stream
      Left err -> return $ Left $ OffsetError $ "Failed to get offset: " <> T.pack (show err)

  resetOffset :: RedisStreamEnv -> ConsumerGroupId -> StartOffset -> RedisStreamT m (Either StreamError ())
  resetOffset env groupId startOffset = do
    -- For Redis, we'd implement this by adjusting the consumer group's position
    -- This is a simplified implementation
    let offset = case startOffset of
          FromBeginning -> StreamOffset "0"
          FromEnd -> StreamOffset "$"
          FromOffset o -> o
          FromTimestamp _ -> StreamOffset "$"  -- Simplified for now

    commitOffset env groupId offset

  replayEvents :: RedisStreamEnv -> ReplayConfig -> RedisStreamT m (Either StreamError ())
  replayEvents env config = RedisStreamT $ liftIO $ do
    -- Simplified replay implementation
    -- In production, this would filter by time range and process in parallel
    return $ Right ()

  getStreamInfo :: RedisStreamEnv -> Text -> RedisStreamT m (Either StreamError StreamInfo)
  getStreamInfo env topic = RedisStreamT $ liftIO $ do
    let streamName = streamKey topic

    result <- runRedis (redisConnection env) $ do
      len <- xlen streamName

      -- Get first and last entries
      firstResult <- xrange streamName "-" "+" (Just 1)
      lastResult <- xrevRange streamName "+" "-" (Just 1)

      -- Get consumer groups
      groupsResult <- xinfoGroups streamName

      return (len, firstResult, lastResult, groupsResult)

    case result of
      (Right len, Right first, Right last, Right groups) -> do
        let firstEntry = case first of
              ((StreamsRecord offset _):_) -> Just (StreamOffset $ T.decodeUtf8 offset, undefined)  -- Would parse timestamp
              _ -> Nothing

            lastEntry = case last of
              ((StreamsRecord offset _):_) -> Just (StreamOffset $ T.decodeUtf8 offset, undefined)
              _ -> Nothing

            consumerGroups = map (\g -> ConsumerGroupId $ T.decodeUtf8 $ xinfoGroupsGroupName g) groups

        return $ Right $ StreamInfo topic len firstEntry lastEntry consumerGroups

      _ -> return $ Left $ ConsumerError "Failed to get stream info"

-- Monad transformer for Redis streaming
newtype RedisStreamT m a = RedisStreamT { runRedisStreamT :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

-- Helper to read with Maybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing
