{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Fractal.Layer
import Control.Category ((>>>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.IORef
import System.Random (randomRIO)

-- Example types for our layers
data Config = Config
  { configPort :: Int
  , configHost :: String
  , configDebug :: Bool
  } deriving (Show, Eq)

data Database = Database
  { dbConnection :: IORef Int  -- Simulated connection
  , dbName :: String
  } deriving (Show)

data Logger = Logger
  { loggerName :: String
  , loggerLevel :: String
  } deriving (Show)

data AppEnv = AppEnv
  { appConfig :: Config
  , appDatabase :: Database
  , appLogger :: Logger
  } deriving (Show)

-- Layer definitions

-- | Configuration layer - pure layer from environment
configLayer :: IO (Layer e Config Config)
configLayer = fromFunction id

-- | Database layer - resource layer with acquisition and release
databaseLayer :: IO (Layer SomeException Config Database)
databaseLayer = fromResource acquire release
  where
    acquire = do
      putStrLn "Acquiring database connection..."
      conn <- newIORef 0
      return $ Database conn "production"

    release db = do
      putStrLn "Releasing database connection..."
      readIORef (dbConnection db) >>= \n ->
        putStrLn $ "  Closed after " ++ show n ++ " queries"

-- | Logger layer - simple IO-based layer
loggerLayer :: IO (Layer e Config Logger)
loggerLayer = fromIO $ do
  putStrLn "Creating logger..."
  return $ Logger "app" "INFO"

-- | Combined app environment layer using parallel composition
appEnvLayer :: IO (Layer SomeException Config AppEnv)
appEnvLayer = do
  -- Get the individual layers
  cfg <- configLayer
  db <- databaseLayer
  log <- loggerLayer
  -- Run config, database, and logger in parallel
  return $ cfg &&& db &&& log
    -- Then combine the results
    >>> fromFunction' (\(c, (d, l)) -> AppEnv c d l)

-- | Example service layer that uses the app environment
serviceLayer :: IO (Layer e AppEnv String)
serviceLayer = fromFunction $ \env ->
  "Service running on " ++ configHost (appConfig env) ++
  ":" ++ show (configPort (appConfig env))

-- | Example of error handling
failingLayer :: IO (Layer String r Int)
failingLayer = fail "Something went wrong!"

safeFailingLayer :: IO (Layer e r (Either String Int))
safeFailingLayer = do
  failing <- failingLayer
  success <- succeed (Left "Recovered from error")
  return $ (succeed' . Right <$> failing) `orElse` success
  where
    (<$>) = mapEnvironment

-- | Example of caching (explicit caching, though already auto-cached)
expensiveComputation :: Layer e r Int
expensiveComputation = fromIO' $ do
  putStrLn "Computing expensive value..."
  threadDelay 1000000  -- 1 second
  randomRIO (1, 100)

-- | Example of retry logic
unreliableService :: IORef Int -> Layer String r String
unreliableService countRef = fromIO' $ do
  count <- atomicModifyIORef countRef (\n -> (n + 1, n))
  if count < 2
    then error "Service temporarily unavailable"
    else return "Service response"

-- | Example using uncached layers for demonstration
uncachedExample :: IO ()
uncachedExample = do
  putStrLn "\n8. Uncached layer demonstration:"

  -- Create an uncached layer that counts invocations
  countRef <- newIORef (0 :: Int)
  let countingLayer = fromIO' $ do
        n <- atomicModifyIORef countRef (\n -> (n + 1, n + 1))
        putStrLn $ "  Layer invoked, count: " ++ show n
        return n

  putStrLn "Running uncached layer twice:"
  r1 <- runLayer countingLayer ()
  r2 <- runLayer countingLayer ()
  putStrLn $ "Results: " ++ show r1 ++ ", " ++ show r2

  -- Now create a cached version
  putStrLn "\nRunning cached version twice:"
  cachedCounting <- cached @Int @() @() countingLayer
  r3 <- runLayer cachedCounting ()
  r4 <- runLayer cachedCounting ()
  putStrLn $ "Results: " ++ show r3 ++ ", " ++ show r4

-- Main demonstration
main :: IO ()
main = do
  putStrLn "=== ZLayer Example ==="

  -- Basic layer composition
  putStrLn "\n1. Basic layer composition:"
  let config = Config 8080 "localhost" True
  appEnv <- appEnvLayer
  result1 <- runLayer appEnv config
  case result1 of
    Left e -> putStrLn $ "Error: " ++ show e
    Right env -> do
      putStrLn $ "Created environment: " ++ show env

      -- Run service with the environment
      service <- serviceLayer
      result2 <- runLayer service env
      case result2 of
        Left e -> putStrLn $ "Service error: " ++ show e
        Right msg -> putStrLn $ "Service: " ++ msg

  -- Error handling
  putStrLn "\n2. Error handling:"
  safeLayer <- safeFailingLayer
  result3 <- runLayer safeLayer ()
  case result3 of
    Left _ -> putStrLn "Unexpected error"
    Right v -> putStrLn $ "Handled error gracefully: " ++ show v

  -- Caching demonstration (now automatic!)
  putStrLn "\n3. Automatic caching demonstration:"

  -- These are automatically cached
  expensiveLayer1 <- fromIO @Int @() @() $ do
    putStrLn "Computing expensive value..."
    threadDelay 1000000  -- 1 second
    randomRIO (1, 100)

  putStrLn "First call (should compute):"
  r1 <- runLayer expensiveLayer1 ()

  putStrLn "Second call (should use cache automatically):"
  r2 <- runLayer expensiveLayer1 ()

  putStrLn $ "Results: " ++ show r1 ++ " and " ++ show r2 ++ " (should be same)"

  -- Parallel execution
  putStrLn "\n4. Parallel execution:"
  slowLayer1 <- fromIO @Int @() @() $ do
    putStrLn "  Starting task 1..."
    threadDelay 1000000
    putStrLn "  Finished task 1"
    return 1

  slowLayer2 <- fromIO @Int @() @() $ do
    putStrLn "  Starting task 2..."
    threadDelay 1500000
    putStrLn "  Finished task 2"
    return 2

  putStrLn "Running two slow tasks in parallel:"
  result4 <- runLayer (slowLayer1 &&& slowLayer2) ()
  case result4 of
    Left e -> putStrLn $ "Parallel error: " ++ show e
    Right (a, b) -> putStrLn $ "Results: " ++ show a ++ ", " ++ show b

  -- Retry logic
  putStrLn "\n5. Retry logic:"
  countRef <- newIORef 0
  let retryableLayer = retry 3 (unreliableService countRef)
  result5 <- runLayer retryableLayer ()
  case result5 of
    Left e -> putStrLn $ "Failed after retries: " ++ e
    Right msg -> putStrLn $ "Success after retries: " ++ msg

  -- Timeout
  putStrLn "\n6. Timeout demonstration:"
  let slowTask = fromIO' @String $ do
        threadDelay 2000000
        return "Completed"

  result6 <- runLayer (timeout 1000000 slowTask) ()
  case result6 of
    Left _ -> putStrLn "Timeout error"
    Right Nothing -> putStrLn "Task timed out"
    Right (Just v) -> putStrLn $ "Task completed: " ++ v

  -- Arrow composition
  putStrLn "\n7. Arrow composition:"
  let add1 = arr @(Layer ()) (+ 1)
      mult2 = arr @(Layer ()) (* 2)
      pipeline = add1 >>> mult2

  result7 <- runLayer pipeline 5
  case result7 of
    Left _ -> putStrLn "Arrow error"
    Right v -> putStrLn $ "5 + 1 then * 2 = " ++ show v

  -- Demonstrate uncached layers
  uncachedExample

  putStrLn "\n=== Example complete ===="
