{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time (UTCTime)
import Data.Time (UTCTime, getCurrentTime)
import Database.Redis (defaultConnectInfo)
import GHC.Generics
import Hasql.Connection as Connection
import Validation

import Fractal.Stream
import Fractal.Stream.Backend.Redis
import Fractal.Stream.Outbox
import Fractal.Stream.Types

-- Example domain events
data TransactionEvent
  = TransactionPosted
    { transactionId :: Text
    , accountId :: Text
    , amount :: Double
    , currency :: Text
    , postedAt :: UTCTime
    }
  | TransactionReversed
    { transactionId :: Text
    , reversalReason :: Text
    , reversedAt :: UTCTime
    }
  deriving (Eq, Show, Generic)

instance ToJSON TransactionEvent
instance FromJSON TransactionEvent

-- Example: Publishing events with exactly-once semantics
publishTransactionWithOutbox :: Connection -> RedisStreamEnv -> TransactionEvent -> IO ()
publishTransactionWithOutbox dbConn streamEnv event = do
  -- Begin database transaction (pseudo-code)
  -- In real code, you'd use hasql transaction support

  -- 1. Make domain changes in database
  -- UPDATE accounts SET balance = balance + amount WHERE ...

  -- 2. Create event envelope
  envelope <- mkEventEnvelope "TransactionPosted" event

  -- 3. Write to outbox in same transaction
  result <- writeToOutbox dbConn "ledger.transactions" Nothing envelope
  case result of
    Left err -> error $ "Failed to write to outbox: " <> show err
    Right _ -> return ()

  -- Commit transaction
  -- The outbox processor will publish to stream asynchronously

-- Example: Simple event publishing (without outbox)
publishTransactionDirect :: RedisStreamEnv -> TransactionEvent -> IO ()
publishTransactionDirect streamEnv event = runRedisStreamT $ do
  eventId <- publishEvent streamEnv "ledger.transactions" "TransactionPosted" event
  liftIO $ putStrLn $ "Published event: " <> show eventId

-- Example: Consuming events
consumeTransactions :: RedisStreamEnv -> IO ()
consumeTransactions streamEnv = runRedisStreamT $ do
  -- Create consumer configuration
  let config = mkStreamConfig ["ledger.transactions"] (ConsumerGroupId "risk-analysis")

  -- Create consumer
  consumerState <- consumer streamEnv config

  -- Subscribe and process events
  subscribeForever streamEnv consumerState $ \(envelope :: EventEnvelope TransactionEvent) -> do
    liftIO $ putStrLn $ "Received event: " <> unpack (_eventType envelope)

    case _payload envelope of
      TransactionPosted{..} -> do
        liftIO $ putStrLn $ "Processing transaction: " <> unpack transactionId
        -- Perform risk analysis
        -- Update risk scores
        return $ pure ()

      TransactionReversed{..} -> do
        liftIO $ putStrLn $ "Processing reversal: " <> unpack transactionId
        -- Update risk model
        return $ pure ()

-- Example: Building a projection
buildAccountBalanceProjection :: RedisStreamEnv -> IO ()
buildAccountBalanceProjection streamEnv = runRedisStreamT $ do
  let config = StreamConfig
        { _streamTopics = ["ledger.transactions"]
        , _streamGroupId = ConsumerGroupId "balance-projection"
        , _streamBatchSize = 100
        , _streamMaxPollInterval = 5000
        , _streamStartOffset = FromBeginning  -- Process all historical events
        }

  consumerState <- consumer streamEnv config

  subscribe streamEnv consumerState $ \(envelope :: EventEnvelope TransactionEvent) -> do
    case _payload envelope of
      TransactionPosted{..} -> do
        -- Update balance projection in database
        liftIO $ putStrLn $ "Updating balance for account: " <> unpack accountId
        return $ pure ()

      TransactionReversed{..} -> do
        -- Reverse the transaction in projection
        liftIO $ putStrLn $ "Reversing transaction: " <> unpack transactionId
        return $ pure ()

-- Example: Main application
main :: IO ()
main = do
  -- Initialize Redis connection
  redisEnv <- createRedisStreamEnv defaultConnectInfo

  -- Initialize database connection for outbox
  dbConnResult <- Connection.acquire "host=localhost port=5432 dbname=outbox user=postgres"
  case dbConnResult of
    Left err -> error $ "Failed to connect to database: " <> show err
    Right dbConn -> do
      -- Create outbox table
      createOutboxTable dbConn

      -- Start outbox processor
      let outboxConfig = OutboxConfig
            { outboxBatchSize = 100
            , outboxPollInterval = 1000
            , outboxMaxRetries = 3
            , outboxRetryDelay = 5000
            , outboxConnection = dbConn
            }

      processor <- runRedisStreamT $
        startOutboxProcessor redisEnv outboxConfig

      -- Start consumer in background
      _ <- forkIO $ consumeTransactions redisEnv

      -- Start projection builder
      _ <- forkIO $ buildAccountBalanceProjection redisEnv

      -- Simulate publishing events
      forever $ do
        now <- getCurrentTime
        let event = TransactionPosted
              { transactionId = "txn_12345"
              , accountId = "acc_67890"
              , amount = 100.0
              , currency = "USD"
              , postedAt = now  -- Would use current time
              }

        -- Publish with exactly-once semantics
        publishTransactionWithOutbox dbConn redisEnv event

        threadDelay 5000000  -- 5 seconds
