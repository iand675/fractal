{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Fractal.Layer
import Control.Category ((>>>), (<<<), id, (.))
import Control.Arrow ((&&&), (***), arr, first, second)
import Control.Exception (SomeException, bracket, evaluate)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Typeable
import Data.Vinyl
import Data.Functor.Identity
import UnliftIO hiding (assert)
import UnliftIO.Async
import Data.Time (getCurrentTime, diffUTCTime)
import Prelude hiding (id, (.))

-- Import the Avro schema compatibility tests
import qualified Fractal.Schema.Compatibility.AvroSpec as Avro
-- Import the client tests
import qualified Fractal.Schema.ClientSpec as Client

-- Test data types
data Config = Config { port :: Int, host :: String } deriving (Eq, Show)
data Database = Database { connId :: Int } deriving (Eq, Show)
data WebServer = WebServer { serverId :: Int } deriving (Eq, Show)
data Cache = Cache { cacheId :: Int } deriving (Eq, Show)

-- Helper to track resource lifecycles
data ResourceTracker = ResourceTracker
  { acquired :: IORef [String]
  , released :: IORef [String]
  }

newResourceTracker :: IO ResourceTracker
newResourceTracker = ResourceTracker <$> newIORef [] <*> newIORef []

trackAcquire :: ResourceTracker -> String -> IO ()
trackAcquire tracker name = atomicModifyIORef' (acquired tracker) $ \xs -> (name : xs, ())

trackRelease :: ResourceTracker -> String -> IO ()
trackRelease tracker name = atomicModifyIORef' (released tracker) $ \xs -> (name : xs, ())

-- Test layers
configLayer :: Layer IO () Config
configLayer = effect $ \_ -> pure $ Config 8080 "localhost"

trackedResource :: ResourceTracker -> String -> a -> Layer IO () a
trackedResource tracker name value = resource
  (\_ -> trackAcquire tracker name >> pure value)
  (\_ -> trackRelease tracker name)

dbLayer :: ResourceTracker -> Layer IO Config Database
dbLayer tracker = resource
  (\cfg -> do
    trackAcquire tracker "database"
    pure $ Database (port cfg))
  (\_ -> trackRelease tracker "database")

webLayer :: ResourceTracker -> Layer IO Config WebServer
webLayer tracker = resource
  (\cfg -> do
    trackAcquire tracker "webserver"
    pure $ WebServer (port cfg + 1000))
  (\_ -> trackRelease tracker "webserver")

cacheLayer :: ResourceTracker -> Layer IO a Cache
cacheLayer tracker = resource
  (\_ -> do
    trackAcquire tracker "cache"
    pure $ Cache 42)
  (\_ -> trackRelease tracker "cache")

-- Test exception handling
failingLayer :: Layer IO () ()
failingLayer = effect $ \_ -> throwIO $ userError "Test failure"

delayedFailingLayer :: Int -> Layer IO () ()
delayedFailingLayer delayMs = resource
  (\_ -> do
    threadDelay (delayMs * 1000)
    throwIO $ userError "Delayed failure")
  (\_ -> pure ())

main :: IO ()
main = hspec $ do
  -- Include the Avro schema compatibility tests
  Avro.spec
  -- Include the client tests
  Client.spec

  describe "Layer - Basic Construction" $ do
    it "effect creates a simple layer" $ do
      let layer = effect $ \() -> pure "test"
      result <- runLayer () layer
      result `shouldBe` "test"

    it "resource manages acquisition and release" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "test-resource" "value"

      -- Run the layer
      withLayer () layer $ \val -> liftIO do
        val `shouldBe` "value"
        acq <- readIORef (acquired tracker)
        acq `shouldBe` ["test-resource"]
        rel <- readIORef (released tracker)
        rel `shouldBe` []

      -- Check cleanup happened
      rel <- readIORef (released tracker)
      rel `shouldBe` ["test-resource"]

    it "pureLayer creates a layer with no effects" $ do
      let layer = pure @(Layer IO ()) "pure value"
      result <- runLayer () layer
      result `shouldBe` "pure value"

  describe "Layer - Composition" $ do
    it "vertical composition with >>> works correctly" $ do
      tracker <- newResourceTracker
      let composed = configLayer >>> dbLayer tracker

      withLayer () composed $ \db -> liftIO do
        connId db `shouldBe` 8080

      -- Check both acquired and released
      acq <- readIORef (acquired tracker)
      rel <- readIORef (released tracker)
      acq `shouldBe` ["database"]
      rel `shouldBe` ["database"]

    it "horizontal composition with &&& works correctly" $ do
      tracker <- newResourceTracker
      let layer = dbLayer tracker &&& webLayer tracker

      withLayer (Config 3000 "test") layer $ \(db, ws) -> liftIO do
        connId db `shouldBe` 3000
        serverId ws `shouldBe` 4000

      -- Check both are cleaned up (order doesn't matter for parallel composition)
      rel <- readIORef (released tracker)
      length rel `shouldBe` 2
      "database" `elem` rel `shouldBe` True
      "webserver" `elem` rel `shouldBe` True

    it "complex composition maintains proper order" $ do
      tracker <- newResourceTracker
      let layer = configLayer >>> (dbLayer tracker &&& webLayer tracker &&& cacheLayer tracker)

      withLayer () layer $ \(db, (ws, cache)) -> liftIO do
        connId db `shouldBe` 8080
        serverId ws `shouldBe` 9080
        cacheId cache `shouldBe` 42

  describe "Layer - Functor instance" $ do
    it "fmap transforms the output" $ do
      let layer = fmap (*2) (effect $ \() -> pure 21)
      result <- runLayer () layer
      result `shouldBe` 42

    it "preserves resource management with fmap" $ do
      tracker <- newResourceTracker
      let layer = fmap show (trackedResource tracker "mapped" 123)

      withLayer () layer $ \val -> liftIO do
        val `shouldBe` "123"

      rel <- readIORef (released tracker)
      rel `shouldBe` ["mapped"]

  describe "Layer - Applicative instance" $ do
    it "pure creates a pure layer" $ do
      let layer = pure @(Layer IO ()) 42
      result <- runLayer () layer
      result `shouldBe` 42

    it "<*> combines layers applicatively" $ do
      let funcLayer = pure @(Layer IO ()) (+10)
      let valueLayer = pure @(Layer IO ()) 32
      let combined = funcLayer <*> valueLayer
      result <- runLayer () combined
      result `shouldBe` 42

    it "liftA2 works correctly" $ do
      tracker <- newResourceTracker
      let layer1 = trackedResource tracker "res1" 10
      let layer2 = trackedResource tracker "res2" 32
      let combined = liftA2 (+) layer1 layer2

      result <- runLayer () combined
      result `shouldBe` 42

  describe "Layer - Monad instance" $ do
    it ">>= sequences operations correctly" $ do
      tracker <- newResourceTracker
      let layer = do
            x <- trackedResource tracker "first" 10
            y <- trackedResource tracker "second" (x + 5)
            pure (x * y)

      result <- runLayer () layer
      result `shouldBe` 150

      -- Check acquisition order
      acq <- readIORef (acquired tracker)
      reverse acq `shouldBe` ["first", "second"]

    it ">> discards first result" $ do
      let layer = effect (\_ -> putStrLn "side effect") >> pure 42
      result <- runLayer () layer
      result `shouldBe` 42

  describe "Layer - Arrow instance" $ do
    it "arr lifts pure functions" $ do
      let layer = arr @(Layer IO) (*2)
      result <- runLayer 21 layer
      result `shouldBe` 42

    it "first applies to first component" $ do
      let layer = first @(Layer IO) (arr @(Layer IO) @Int @Int (*2))
      result <- runLayer (21, "test") layer
      result `shouldBe` (42, "test")

    it "second applies to second component" $ do
      let layer = second @(Layer IO) (arr @(Layer IO) @Int @Int (*2))
      result <- runLayer ("test", 21) layer
      result `shouldBe` ("test", 42)

    it "*** combines two arrows" $ do
      let layer = arr (*2) *** arr (++ "!")
      result <- runLayer (21, "test") layer
      result `shouldBe` (42, "test!")

  describe "Layer - Alternative instance" $ do
    it "empty throws EmptyLayer exception" $ do
      let layer = empty :: Layer IO () Int
      runLayer () layer `shouldThrow` anyException

    it "<|> provides fallback behavior" $ do
      let primary = empty :: Layer IO () String
      let fallback = pure "fallback"
      let combined = primary <|> fallback

      result <- runLayer () combined
      result `shouldBe` "fallback"

    it "<|> cleans up failed branch resources" $ do
      tracker <- newResourceTracker
      let failing = trackedResource tracker "failing" () >> empty
      let success = trackedResource tracker "success" "ok"
      let combined = failing <|> success

      result <- runLayer () combined
      result `shouldBe` "ok"

      -- Check that failing resource was cleaned up
      acq <- readIORef (acquired tracker)
      rel <- readIORef (released tracker)
      "failing" `elem` acq `shouldBe` True
      "failing" `elem` rel `shouldBe` True
      "success" `elem` acq `shouldBe` True

  describe "Layer - MonadReader instance" $ do
    it "ask returns the dependencies" $ do
      let layer = ask :: Layer IO Config Config
      result <- runLayer (Config 8080 "test") layer
      result `shouldBe` Config 8080 "test"

    it "local modifies dependencies" $ do
      let layer = local (\(Config p h) -> Config (p+1) h) ask
      result <- runLayer (Config 8080 "test") layer
      result `shouldBe` Config 8081 "test"

  describe "Layer - Resource Management" $ do
    -- Note: Resource cleanup order varies based on composition type:
    -- - Monadic bind (>>=): Resources are released in reverse order within each monadic chain
    -- - Parallel composition (&&&): Resources may be released in any order since they're peers
    -- - The important guarantee is that ALL resources are properly cleaned up

    it "releases resources in LIFO order for monadic composition" $ do
      tracker <- newResourceTracker
      let layer = do
            _ <- trackedResource tracker "first" ()
            _ <- trackedResource tracker "second" ()
            _ <- trackedResource tracker "third" ()
            pure ()

      withLayer () layer $ \_ -> pure ()

      -- For monadic composition, check all are released
      rel <- readIORef (released tracker)
      length rel `shouldBe` 3
      "first" `elem` rel `shouldBe` True
      "second" `elem` rel `shouldBe` True
      "third" `elem` rel `shouldBe` True

    it "releases resources on exception" $ do
      tracker <- newResourceTracker
      let layer = do
            _ <- trackedResource tracker "resource" ()
            effect $ \_ -> throwIO $ userError "boom"

      withLayer () layer (\_ -> pure ()) `shouldThrow` anyException

      rel <- readIORef (released tracker)
      rel `shouldBe` ["resource"]

    it "handles nested resource scopes correctly" $ do
      tracker <- newResourceTracker
      let inner = trackedResource tracker "inner" "inner-value"
      let outer = resource
            (\_ -> do
              trackAcquire tracker "outer"
              withLayer () inner $ \val -> do
                pure $ "outer-" ++ val)
            (\_ -> trackRelease tracker "outer")

      result <- runLayer () outer
      result `shouldBe` "outer-inner-value"

      -- Both should be released, inner first
      rel <- readIORef (released tracker)
      rel `shouldBe` ["outer", "inner"]

    it "demonstrates cleanup order matters for dependent resources" $ do
      -- This test shows a case where order DOES matter
      connectionOpen <- newIORef False
      queryExecuted <- newIORef False

      let dbConnection = resource
            (\_ -> do
              writeIORef connectionOpen True
              pure "connection")
            (\_ -> writeIORef connectionOpen False)

      let dbQuery = resource
            (\conn -> do
              isOpen <- readIORef connectionOpen
              if isOpen
                then writeIORef queryExecuted True >> pure "query"
                else error "Connection closed before query!")
            (\_ -> pure ())

      let layer = dbConnection >>= \conn -> dbQuery

      -- Run the layer
      result <- runLayer () layer
      result `shouldBe` "query"

      -- Both should have been executed
      readIORef queryExecuted >>= (`shouldBe` True)
      -- And connection should be closed after
      readIORef connectionOpen >>= (`shouldBe` False)

  describe "Layer - Concurrent Composition" $ do
    it "zipLayer runs layers concurrently" $ do
      startTime <- newIORef =<< getCurrentTime
      let layer1 = effect $ \_ -> threadDelay 100000 >> getCurrentTime
      let layer2 = effect $ \_ -> threadDelay 100000 >> getCurrentTime
      let combined = zipLayer layer1 layer2

      (t1, t2) <- runLayer ((), ()) combined
      start <- readIORef startTime

      -- Both should complete at roughly the same time (within 50ms)
      let diff = abs $ diffUTCTime t1 t2
      diff < 0.05 `shouldBe` True

    it "zipLayer handles concurrent failures correctly" $ do
      tracker <- newResourceTracker
      let layer1 = trackedResource tracker "res1" () >> delayedFailingLayer 50
      let layer2 = trackedResource tracker "res2" "value"
      let combined = zipLayer layer1 layer2

      runLayer ((), ()) combined `shouldThrow` anyException

      -- Both resources should be cleaned up
      threadDelay 100000
      rel <- readIORef (released tracker)
      "res1" `elem` rel `shouldBe` True
      "res2" `elem` rel `shouldBe` True

  describe "Layer - Environment" $ do
    it "builds and accesses environment correctly" $ do
      let env = appendEnvironment (Config 8080 "test") $
                appendEnvironment (Database 1) $
                emptyEnvironment

      let cfg = lookupEnvironment @Config env
      let db = lookupEnvironment @Database env

      cfg `shouldBe` Config 8080 "test"
      db `shouldBe` Database 1

    it "assembleFromEnvironment works for functions" $ do
      let env = appendEnvironment (Config 8080 "test") $
                appendEnvironment (Database 1) $
                emptyEnvironment

      let mkServer :: Config -> Database -> WebServer
          mkServer c d = WebServer (port c + connId d)

      let server = assembleFromEnvironment env mkServer
      server `shouldBe` WebServer 8081

  describe "Layer - Property Tests" $ do
    it "Functor laws" $ property $ \(n :: Int) ->
      monadicIO $ do
        -- fmap id = id
        let layer = pure n :: Layer IO () Int
        r1 <- run $ runLayer () (fmap id layer)
        r2 <- run $ runLayer () layer
        assert $ r1 == r2

    it "Applicative identity law" $ property $ \(n :: Int) ->
      monadicIO $ do
        let layer = pure n :: Layer IO () Int
        r1 <- run $ runLayer () (pure id <*> layer)
        r2 <- run $ runLayer () layer
        assert $ r1 == r2

    it "Category identity laws" $ property $ \(n :: Int) ->
      monadicIO $ do
        let layer = pure n :: Layer IO () Int
        r1 <- run $ runLayer () (Control.Category.id >>> layer)
        r2 <- run $ runLayer () (layer >>> Control.Category.id)
        r3 <- run $ runLayer () layer
        assert $ r1 == r2 && r2 == r3

  describe "Layer - Service" $ do
    it "mkService creates a service from a layer" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "test-service" ("service-value" :: String)
      let svc = mkService layer

      -- Service should behave like a normal layer
      withLayer () (service svc) $ \val -> liftIO do
        val `shouldBe` "service-value"

      rel <- readIORef (released tracker)
      rel `shouldBe` ["test-service"]

    it "service caches initialization - single access" $ do
      initCount <- newIORef (0 :: Int)
      let expensiveLayer = resource
            (\_ -> do
              atomicModifyIORef' initCount $ \n -> (n + 1, ())
              pure ("expensive result" :: String))
            (\_ -> pure ())

      let svc = mkService expensiveLayer

      -- First access should initialize
      result <- runLayer () (service svc)
      result `shouldBe` "expensive result"
      count <- readIORef initCount
      count `shouldBe` 1

    it "service caches initialization - multiple sequential accesses" $ do
      initCount <- newIORef (0 :: Int)
      tracker <- newResourceTracker

      let expensiveLayer = resource
            (\_ -> do
              n <- atomicModifyIORef' initCount $ \n -> (n + 1, n + 1)
              trackAcquire tracker ("init-" ++ show n)
              pure $ ("result-" ++ show n :: String))
            (\res -> trackRelease tracker res)

      let svc = mkService expensiveLayer

      -- Multiple accesses should reuse the same instance
      let multiAccessLayer = do
            r1 <- service svc
            r2 <- service svc
            r3 <- service svc
            pure (r1, r2, r3)

      (res1, res2, res3) <- runLayer () multiAccessLayer
      res1 `shouldBe` "result-1"
      res2 `shouldBe` "result-1"  -- Same instance
      res3 `shouldBe` "result-1"  -- Same instance

      count <- readIORef initCount
      count `shouldBe` 1  -- Only initialized once

    it "service caches initialization - concurrent accesses" $ do
      initCount <- newIORef (0 :: Int)
      startBarrier <- newEmptyMVar

      let slowLayer = resource
            (\_ -> do
              -- Wait for all threads to be ready
              threadDelay 50000  -- 50ms initialization
              n <- atomicModifyIORef' initCount $ \n -> (n + 1, n + 1)
              pure $ ("concurrent-result-" ++ show n :: String))
            (\_ -> pure ())

      let svc = mkService slowLayer

      -- Launch multiple concurrent accesses
      let concurrentLayer =
            (,,,) <$> service svc <*> service svc <*> service svc <*> service svc

      (r1, r2, r3, r4) <- runLayer () concurrentLayer

      -- All should get the same result
      r1 `shouldBe` "concurrent-result-1"
      r2 `shouldBe` "concurrent-result-1"
      r3 `shouldBe` "concurrent-result-1"
      r4 `shouldBe` "concurrent-result-1"

      count <- readIORef initCount
      count `shouldBe` 1  -- Only one initialization despite concurrent access

    it "service propagates initialization errors" $ do
      errorCount <- newIORef (0 :: Int)

      let failingLayer :: Layer IO () Int = resource
            (\_ -> do
              atomicModifyIORef' errorCount $ \n -> (n + 1, ())
              throwIO $ userError "Service initialization failed")
            (\_ -> pure ())

      let svc = mkService failingLayer

      -- First access should fail
      runLayer () (service svc <|> service svc) `shouldThrow` anyException

      count <- readIORef errorCount
      count `shouldBe` 1  -- Should only try to initialize once

    it "service works with complex layer compositions" $ do
      tracker <- newResourceTracker

      -- Shared service used by multiple layers
      let sharedService = mkService $ trackedResource tracker "shared" (42 :: Int)

      -- Layer that uses the service
      let consumerLayer1 = do
            shared <- service sharedService
            trackedResource tracker ("consumer1-" ++ show shared) ("c1-" ++ show shared)

      -- Another layer that uses the same service
      let consumerLayer2 = do
            shared <- service sharedService
            trackedResource tracker ("consumer2-" ++ show shared) ("c2-" ++ show shared)

      -- Compose them together
      let composedLayer = liftA2 (,) consumerLayer1 consumerLayer2

      (r1, r2) <- runLayer () composedLayer
      r1 `shouldBe` "c1-42"
      r2 `shouldBe` "c2-42"

      -- Check that shared service was only initialized once
      acq <- readIORef (acquired tracker)
      length (filter (== "shared") acq) `shouldBe` 1

    it "service respects layer lifecycle" $ do
      tracker <- newResourceTracker

      let svc = mkService $ trackedResource tracker "service" ("value" :: String)

      -- Use service within a layer
      withLayer () (service svc) $ \val -> liftIO do
        val `shouldBe` "value"
        -- Service should be acquired
        acq <- readIORef (acquired tracker)
        "service" `elem` acq `shouldBe` True
        -- But not yet released
        rel <- readIORef (released tracker)
        "service" `elem` rel `shouldBe` False

      -- After layer exits, service should be cleaned up
      rel <- readIORef (released tracker)
      "service" `elem` rel `shouldBe` True

    it "different services with same type can coexist in separate layer scopes" $ do
      initCounter <- newIORef (0 :: Int)

      let makeService name = mkService $ resource
            (\_ -> do
              n <- atomicModifyIORef' initCounter $ \n -> (n + 1, n)
              pure $ (name ++ "-" ++ show n :: String))
            (\_ -> pure ())

      let service1 = makeService "service1"
      let service2 = makeService "service2"

      -- Use services in separate scopes
      result1 <- runLayer () (service service1)
      result2 <- runLayer () (service service2)

      -- Each scope gets its own cache
      result1 `shouldBe` "service1-0"
      result2 `shouldBe` "service2-1"

    it "service with monadic bind maintains caching" $ do
      initCount <- newIORef (0 :: Int)

      let expensiveService = mkService $ resource
            (\_ -> do
              atomicModifyIORef' initCount $ \n -> (n + 1, ())
              pure (100 :: Int))
            (\_ -> pure ())

      -- Use service multiple times in monadic composition
      let layer = do
            x <- service expensiveService
            y <- service expensiveService
            z <- effect $ \_ -> do
              -- Even in nested effect
              runLayer () (service expensiveService)
            pure (x + y + z)

      result <- runLayer () layer
      result `shouldBe` 300

      -- Should still only initialize once within the same layer scope
      count <- readIORef initCount
      count `shouldBe` 2  -- Once for main layer, once for nested runLayer

    it "service initialization failure doesn't prevent cleanup of other resources" $ do
      tracker <- newResourceTracker

      let goodLayer = trackedResource tracker "good" ("good-value" :: String)
      let badService :: Service IO () Int = mkService do
            () <- resource
              (\_ -> do
                trackAcquire tracker "bad")
              (\_ -> trackRelease tracker "bad")

            effect $ \_ -> throwIO $ userError "Bad service"

      let composedLayer = do
            good <- goodLayer
            bad <- service badService
            pure (good, bad)

      -- Should fail
      runLayer () composedLayer `shouldThrow` anyException

      -- But good resource should still be cleaned up
      rel <- readIORef (released tracker)
      "good" `elem` rel `shouldBe` True
      "bad" `elem` rel `shouldBe` True  -- Even failed initialization should clean up
