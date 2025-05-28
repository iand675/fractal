# Fractal Stream

A high-level event streaming library for Haskell that provides a unified interface for event sourcing with support for both Redis (local development) and Kinesis (production).

## Features

- **Unified streaming interface** - Same API works with Redis Streams locally and Kinesis in production
- **Event sourcing patterns** - First-class support for event sourcing and CQRS
- **Exactly-once delivery** - Transactional outbox pattern with Hasql integration
- **Schema versioning** - Built-in support for evolving event schemas
- **Railway-oriented error handling** - Explicit error handling without silent failures
- **Projections and read models** - Easy construction of materialized views from event streams
- **Replay and reprocessing** - Full event history replay for recovery and new projections

## Architecture

The library implements the event streaming architecture outlined in the vision document:

```haskell
-- Event structure follows the "sentence" pattern
data TransactionEvent = TransactionPosted
  { userId :: UserId              -- Subject: "who"
  , action :: Text                -- Verb: "what happened"
  , amount :: Money               -- Direct object
  , merchant :: Text              -- Context
  , cardId :: CardId              -- Additional context
  }
```

## Quick Start

### Basic Event Publishing

```haskell
import Fractal.Stream
import Fractal.Stream.Backend.Redis

-- Initialize Redis backend
redisEnv <- createRedisStreamEnv defaultConnectInfo

-- Publish an event
runRedisStreamT $ do
  eventId <- publishEvent redisEnv "ledger.transactions" "TransactionPosted" 
    TransactionPosted 
      { transactionId = "txn_123"
      , amount = 100.0
      , accountId = "acc_456"
      }
```

### Consuming Events

```haskell
-- Create consumer configuration
let config = mkStreamConfig ["ledger.transactions"] (ConsumerGroupId "fraud-detection")

-- Subscribe to events
runRedisStreamT $ do
  consumer <- consumer redisEnv config
  
  subscribeForever redisEnv consumer $ \envelope -> do
    case _payload envelope of
      TransactionPosted{..} -> do
        -- Process transaction
        performFraudCheck transactionId amount
        return $ Success ()
```

### Exactly-Once Delivery with Outbox Pattern

```haskell
-- In a database transaction:
publishTransactionWithOutbox dbConn redisEnv event = do
  -- 1. Make domain changes
  updateAccountBalance accountId amount
  
  -- 2. Write event to outbox in same transaction
  envelope <- mkEventEnvelope "TransactionPosted" event
  writeToOutbox dbConn "ledger.transactions" Nothing envelope
  
  -- 3. Commit transaction
  -- Outbox processor publishes asynchronously
```

### Building Projections

```haskell
buildBalanceProjection streamEnv = runRedisStreamT $ do
  let config = StreamConfig
        { _streamTopics = ["ledger.transactions"]
        , _streamGroupId = ConsumerGroupId "balance-projection"
        , _streamStartOffset = FromBeginning  -- Replay all events
        }
  
  consumer <- consumer streamEnv config
  
  subscribe streamEnv consumer $ \envelope -> do
    updateProjection (_payload envelope)
    return $ Success ()
```

## Topic Naming Convention

Topics follow the pattern: `{domain}.{bounded-context}`

- `ledger.transactions` - Transaction events in the ledger domain
- `cards.authorizations` - Card authorization events
- `payments.transfers` - Payment transfer events

## Error Handling

The library uses railway-oriented programming for explicit error handling:

```haskell
processEvent :: EventEnvelope e -> Either ProcessingError ProcessedEvent
processEvent envelope =
  validateSchema envelope >>= \validEvent ->
  applyBusinessRules validEvent >>= \domainEvent ->
  persistProjection domainEvent
```

No errors are silently dropped - all failures are explicitly handled and observable.

## Local Development

The library automatically uses Redis Streams for local development:

```bash
# Set environment variable
export ENABLE_REDIS_LOCALLY=1

# Redis is started automatically when needed
```

## Production Deployment

In production, configure Kinesis backend:

```haskell
-- Initialize Kinesis backend (to be implemented)
kinesisEnv <- createKinesisStreamEnv kinesisConfig
```

## Storage Tiers

Events are automatically managed across storage tiers:

1. **Hot tier** - Recent events in Redis/Kinesis (7-30 days)
2. **Warm tier** - Archived to S3 in Avro format (1 year)
3. **Cold tier** - Long-term storage in S3 Glacier (7+ years)

## Contributing

See CONTRIBUTING.md for development setup and guidelines.

## License

BSD-3-Clause
