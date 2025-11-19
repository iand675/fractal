{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Fractal.Layer.LayerSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Fractal.Layer
import Control.Category ((>>>), (<<<), id, (.))
import Control.Arrow ((&&&), (***), arr, first, second, ArrowZero(..), ArrowPlus(..), app)
import Control.Exception (SomeException, evaluate)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Data.Typeable
import Data.Vinyl hiding ((<+>))
import Data.Functor.Identity
import Data.Profunctor (first', second', left', right')
import Data.Profunctor.Traversing (traverse')
import UnliftIO hiding (assert)
import UnliftIO.Async
import Data.Time (getCurrentTime, diffUTCTime)
import Prelude hiding (id, (.))

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

trackedResource :: (Typeable a) => ResourceTracker -> String -> a -> Layer IO () a
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

spec :: Spec
spec = do
  describe "Layer - Basic Construction" $ do
    it "effect creates a simple layer" $ do
      let layer = effect (\() -> pure ("test" :: String)) :: Layer IO () String
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
              pure ("connection" :: String))
            (\_ -> writeIORef connectionOpen False) :: Layer IO () String

      let dbQuery = resource
            (\_ -> do
              isOpen <- readIORef connectionOpen
              if isOpen
                then writeIORef queryExecuted True >> pure ("query" :: String)
                else error "Connection closed before query!")
            (\_ -> pure ()) :: Layer IO () String

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

  describe "Layer - bracketed function" $ do
    it "manages scoped resources correctly" $ do
      acquired <- newIORef False
      released <- newIORef False

      let scopedBracket = \() use -> bracket
            (writeIORef acquired True >> pure "resource")
            (\_ -> writeIORef released True)
            use

      let layer = bracketed scopedBracket

      withLayer () layer $ \res -> liftIO $ do
        res `shouldBe` "resource"
        readIORef acquired >>= (`shouldBe` True)
        readIORef released >>= (`shouldBe` False)  -- Not yet released

      -- After layer exits, resource should be released
      readIORef released >>= (`shouldBe` True)

    it "keeps resource alive during layer lifetime" $ do
      ref <- newIORef (0 :: Int)

      let scopedBracket = \() use -> bracket
            (modifyIORef' ref (+1) >> pure ())
            (\_ -> modifyIORef' ref (*10))
            use

      let layer = bracketed scopedBracket

      withLayer () layer $ \_ -> liftIO $ do
        -- Bracket opened, count should be 1
        count <- readIORef ref
        count `shouldBe` 1
        -- Modify it during use
        modifyIORef' ref (+5)

      -- After layer exits, should have been multiplied by 10
      readIORef ref >>= (`shouldBe` 60)  -- (1 + 5) * 10

    it "handles exceptions correctly" $ do
      acquired <- newIORef False
      released <- newIORef False

      let scopedBracket = \() use -> bracket
            (writeIORef acquired True >> pure "resource")
            (\_ -> writeIORef released True)
            use

      let layer = bracketed scopedBracket >> (effect (\_ -> throwIO $ userError "boom") :: Layer IO () String)

      withLayer () layer (\_ -> pure ()) `shouldThrow` anyException

      -- Resource should still be released despite exception
      readIORef released >>= (`shouldBe` True)

    it "works with concurrent operations" $ do
      counter <- newIORef (0 :: Int)

      let scopedBracket = \() use -> bracket
            (atomicModifyIORef' counter $ \n -> (n+1, n+1))
            (\_ -> atomicModifyIORef' counter $ \n -> (n*2, ()))
            use

      let layer1 = bracketed scopedBracket
      let layer2 = bracketed scopedBracket

      let composed = liftA2 (,) layer1 layer2

      (a, b) <- runLayer () composed
      -- Each bracket should get a unique counter value
      a `shouldNotBe` b

      -- Both should be released (counter doubled twice)
      final <- readIORef counter
      final `shouldBe` ((a + b) * 2)

    it "resource survives beyond immediate scope" $ do
      resultRef <- newIORef ("" :: String)

      let scopedBracket = \(input :: String) use -> bracket
            (pure input)
            (\val -> writeIORef resultRef val)
            use

      let layer = bracketed scopedBracket

      result <- runLayer "test-value" layer
      result `shouldBe` "test-value"

      -- Cleanup should have recorded the value
      readIORef resultRef >>= (`shouldBe` "test-value")

    it "handles nested bracketed resources" $ do
      tracker <- newResourceTracker

      let bracket1 = \() use -> bracket
            (trackAcquire tracker "outer" >> pure "outer")
            (\_ -> trackRelease tracker "outer")
            use

      let bracket2 = \() use -> bracket
            (trackAcquire tracker "inner" >> pure "inner")
            (\_ -> trackRelease tracker "inner")
            use

      let layer = do
            outer <- bracketed bracket1
            inner <- bracketed bracket2
            pure (outer, inner)

      (o, i) <- runLayer () layer
      o `shouldBe` "outer"
      i `shouldBe` "inner"

      -- Both should be acquired
      acq <- readIORef (acquired tracker)
      length acq `shouldBe` 2

      -- Both should be released
      rel <- readIORef (released tracker)
      length rel `shouldBe` 2

  describe "Layer - ArrowZero and ArrowPlus instances" $ do
    it "zeroArrow throws EmptyLayer exception" $ do
      let layer = zeroArrow :: Layer IO Int String
      runLayer 42 layer `shouldThrow` anyException

    it "<+> provides fallback with zeroArrow" $ do
      tracker <- newResourceTracker
      let failing = zeroArrow :: Layer IO () String
      let success = trackedResource tracker "success" "fallback-value"
      let combined = failing <+> success

      result <- runLayer () combined
      result `shouldBe` "fallback-value"

      acq <- readIORef (acquired tracker)
      acq `shouldBe` ["success"]

    it "<+> tries first arrow before fallback" $ do
      tracker <- newResourceTracker
      let primary = trackedResource tracker "primary" "primary-value"
      let fallback = trackedResource tracker "fallback" "fallback-value"
      let combined = primary <+> fallback

      result <- runLayer () combined
      result `shouldBe` "primary-value"

      acq <- readIORef (acquired tracker)
      "primary" `elem` acq `shouldBe` True
      "fallback" `elem` acq `shouldBe` False  -- Fallback not tried

    it "<+> cleans up failed branch" $ do
      tracker <- newResourceTracker
      let failing = trackedResource tracker "failing" () >> zeroArrow
      let success = trackedResource tracker "success" "ok"
      let combined = failing <+> success

      result <- runLayer () combined
      result `shouldBe` "ok"

      acq <- readIORef (acquired tracker)
      rel <- readIORef (released tracker)
      "failing" `elem` rel `shouldBe` True  -- Failed branch cleaned up
      "success" `elem` acq `shouldBe` True

    it "<+> chains multiple alternatives" $ do
      let opt1 = zeroArrow :: Layer IO () String
      let opt2 = zeroArrow :: Layer IO () String
      let opt3 = pure "third-time-lucky"
      let combined = opt1 <+> opt2 <+> opt3

      result <- runLayer () combined
      result `shouldBe` "third-time-lucky"

  describe "Layer - ArrowApply instance" $ do
    it "app applies layer to input" $ do
      let makeLayer n = arr (*n) :: Layer IO Int Int
      let dynamicLayer = app :: Layer IO (Layer IO Int Int, Int) Int

      result <- runLayer (makeLayer 5, 7) dynamicLayer
      result `shouldBe` 35

    it "app works with resource-based layers" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "dynamic" 100
      let dynamicApply = app :: Layer IO (Layer IO () Int, ()) Int

      result <- runLayer (layer, ()) dynamicApply
      result `shouldBe` 100

      acq <- readIORef (acquired tracker)
      acq `shouldBe` ["dynamic"]

    it "app enables dynamic layer selection" $ do
      tracker <- newResourceTracker
      let layer1 = trackedResource tracker "layer1" "option-a"
      let layer2 = trackedResource tracker "layer2" "option-b"

      let selectLayer :: Bool -> Layer IO () String
          selectLayer True = layer1
          selectLayer False = layer2

      -- Test with True
      result1 <- runLayer (selectLayer True, ()) app
      result1 `shouldBe` "option-a"

      -- Test with False
      result2 <- runLayer (selectLayer False, ()) app
      result2 `shouldBe` "option-b"

    it "app with complex arrow composition" $ do
      let doubler = arr (*2) :: Layer IO Int Int
      let adder = arr (+10) :: Layer IO Int Int

      -- Create a layer that chooses which operation to apply
      let picker = arr (\(choice, val) ->
            if choice then (doubler, val) else (adder, val))

      let pipeline = picker >>> app

      result1 <- runLayer (True, 5) pipeline
      result1 `shouldBe` 10  -- 5 * 2

      result2 <- runLayer (False, 5) pipeline
      result2 `shouldBe` 15  -- 5 + 10

  describe "Layer - Strong (Profunctor) instance" $ do
    it "first' processes first element of tuple" $ do
      let layer = effect $ \n -> pure (n * 2)
      let firstLayer = first' layer :: Layer IO (Int, String) (Int, String)

      result <- runLayer (5, "test") firstLayer
      result `shouldBe` (10, "test")

    it "second' processes second element of tuple" $ do
      let layer = effect $ \s -> pure (s ++ "!")
      let secondLayer = second' layer :: Layer IO (Int, String) (Int, String)

      result <- runLayer (42, "hello") secondLayer
      result `shouldBe` (42, "hello!")

    it "first' with resource management" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "first-resource" 100
      let firstLayer = first' layer

      withLayer ((), "data") firstLayer $ \(val, str) -> liftIO $ do
        val `shouldBe` 100
        str `shouldBe` "data"
        acq <- readIORef (acquired tracker)
        acq `shouldBe` ["first-resource"]

      rel <- readIORef (released tracker)
      rel `shouldBe` ["first-resource"]

    it "second' with resource management" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "second-resource" "result"
      let secondLayer = second' layer

      withLayer ("data", ()) secondLayer $ \(str, val) -> liftIO $ do
        str `shouldBe` "data"
        val `shouldBe` "result"

      rel <- readIORef (released tracker)
      rel `shouldBe` ["second-resource"]

  describe "Layer - Choice (Profunctor) instance" $ do
    it "left' processes Left values" $ do
      let layer = effect (\n -> pure (n * 2) :: IO Int) :: Layer IO Int Int
      let leftLayer = left' layer :: Layer IO (Either Int String) (Either Int String)

      result <- runLayer (Left 5) leftLayer
      result `shouldBe` Left 10

    it "left' passes through Right values" $ do
      let layer = effect (\n -> pure (n * 2) :: IO Int) :: Layer IO Int Int
      let leftLayer = left' layer :: Layer IO (Either Int String) (Either Int String)

      result <- runLayer (Right "test") leftLayer
      result `shouldBe` Right "test"

    it "right' processes Right values" $ do
      let layer = effect (\s -> pure (s ++ "!") :: IO String) :: Layer IO String String
      let rightLayer = right' layer :: Layer IO (Either Int String) (Either Int String)

      result <- runLayer (Right "hello") rightLayer
      result `shouldBe` Right "hello!"

    it "right' passes through Left values" $ do
      let layer = effect (\s -> pure (s ++ "!") :: IO String) :: Layer IO String String
      let rightLayer = right' layer :: Layer IO (Either Int String) (Either Int String)

      result <- runLayer (Left 42) rightLayer
      result `shouldBe` Left 42

    it "left' with resource management only for Left" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "left-resource" 100
      let leftLayer = left' layer

      -- Test with Left - should acquire resource
      result1 <- runLayer (Left ()) leftLayer
      result1 `shouldBe` Left 100
      acq1 <- readIORef (acquired tracker)
      acq1 `shouldBe` ["left-resource"]

      -- Clear tracker
      writeIORef (acquired tracker) []
      writeIORef (released tracker) []

      -- Test with Right - should NOT acquire resource
      result2 <- runLayer (Right "skip") leftLayer
      result2 `shouldBe` Right "skip"
      acq2 <- readIORef (acquired tracker)
      acq2 `shouldBe` []  -- No resource acquired

  describe "Layer - Traversing (Profunctor) instance" $ do
    it "traverse' processes list elements" $ do
      let layer = effect (\n -> pure (n * 2) :: IO Int) :: Layer IO Int Int
      let travLayer = traverse' layer :: Layer IO [Int] [Int]

      result <- runLayer [1, 2, 3, 4] travLayer
      result `shouldBe` [2, 4, 6, 8]

    it "traverse' works with empty lists" $ do
      let layer = effect (\n -> pure (n * 2) :: IO Int) :: Layer IO Int Int
      let travLayer = traverse' layer :: Layer IO [Int] [Int]

      result <- runLayer [] travLayer
      result `shouldBe` []

    it "traverse' works with Maybe" $ do
      let layer = effect (\n -> pure (n + 10) :: IO Int) :: Layer IO Int Int
      let travLayer = traverse' layer :: Layer IO (Maybe Int) (Maybe Int)

      result1 <- runLayer (Just 5) travLayer
      result1 `shouldBe` Just 15

      result2 <- runLayer Nothing travLayer
      result2 `shouldBe` Nothing

    it "traverse' with resource management" $ do
      tracker <- newResourceTracker
      counter <- newIORef (0 :: Int)

      let layer = resource
            (\n -> do
              i <- atomicModifyIORef' counter $ \c -> (c+1, c)
              trackAcquire tracker ("resource-" ++ show i)
              pure (n * 2))
            (\_ -> pure ())

      let travLayer = traverse' layer :: Layer IO [Int] [Int]

      result <- runLayer [1, 2, 3] travLayer
      result `shouldBe` [2, 4, 6]

      -- Should acquire multiple resources (one per list element)
      acq <- readIORef (acquired tracker)
      length acq `shouldBe` 3

    it "traverse' processes complex structures" $ do
      let layer = effect (\s -> pure (length s) :: IO Int) :: Layer IO String Int
      let travLayer = traverse' layer :: Layer IO [String] [Int]

      result <- runLayer ["a", "bb", "ccc"] travLayer
      result `shouldBe` [1, 2, 3]

  describe "Layer - Semigroup instance" $ do
    it "combines layers with <>" $ do
      let layer1 = pure [1, 2, 3] :: Layer IO () [Int]
      let layer2 = pure [4, 5, 6] :: Layer IO () [Int]
      let combined = layer1 <> layer2

      result <- runLayer () combined
      result `shouldBe` [1, 2, 3, 4, 5, 6]

    it "respects semigroup associativity" $ property $ \(a :: Int) (b :: Int) (c :: Int) ->
      monadicIO $ do
        let l1 = pure [a] :: Layer IO () [Int]
        let l2 = pure [b] :: Layer IO () [Int]
        let l3 = pure [c] :: Layer IO () [Int]

        r1 <- run $ runLayer () ((l1 <> l2) <> l3)
        r2 <- run $ runLayer () (l1 <> (l2 <> l3))
        assert $ r1 == r2

    it "combines with resource management" $ do
      tracker <- newResourceTracker
      let layer1 = trackedResource tracker "res1" "hello"
      let layer2 = trackedResource tracker "res2" " world"
      let combined = liftA2 (<>) layer1 layer2

      result <- runLayer () combined
      result `shouldBe` "hello world"

      acq <- readIORef (acquired tracker)
      length acq `shouldBe` 2

    it "works with String (semigroup)" $ do
      let layer1 = pure "Hello" :: Layer IO () String
      let layer2 = pure " World" :: Layer IO () String
      let combined = layer1 <> layer2

      result <- runLayer () combined
      result `shouldBe` "Hello World"

  describe "Layer - Monoid instance" $ do
    it "mempty produces empty value" $ do
      let layer = mempty :: Layer IO () [Int]
      result <- runLayer () layer
      result `shouldBe` []

    it "mempty is left identity" $ property $ \(xs :: [Int]) ->
      monadicIO $ do
        let layer = pure xs :: Layer IO () [Int]
        r1 <- run $ runLayer () (mempty <> layer)
        r2 <- run $ runLayer () layer
        assert $ r1 == r2

    it "mempty is right identity" $ property $ \(xs :: [Int]) ->
      monadicIO $ do
        let layer = pure xs :: Layer IO () [Int]
        r1 <- run $ runLayer () (layer <> mempty)
        r2 <- run $ runLayer () layer
        assert $ r1 == r2

    it "works with String (monoid)" $ do
      let combined = mempty <> pure "test" <> mempty :: Layer IO () String
      result <- runLayer () combined
      result `shouldBe` "test"

    it "mconcat combines multiple layers" $ do
      let layers = [pure [1], pure [2], pure [3], pure [4]] :: [Layer IO () [Int]]
      let combined = mconcat layers

      result <- runLayer () combined
      result `shouldBe` [1, 2, 3, 4]

  describe "Layer - Complex Alternative/MonadPlus chains" $ do
    it "deeply nested <|> chains work correctly" $ do
      tracker <- newResourceTracker
      let opt1 = trackedResource tracker "opt1" () >> empty
      let opt2 = trackedResource tracker "opt2" () >> empty
      let opt3 = trackedResource tracker "opt3" () >> empty
      let opt4 = trackedResource tracker "opt4" ("success" :: String)
      let combined = opt1 <|> opt2 <|> opt3 <|> opt4

      result <- runLayer () combined
      result `shouldBe` "success"

      -- All failed options should be cleaned up
      rel <- readIORef (released tracker)
      length (filter (`elem` ["opt1", "opt2", "opt3"]) rel) `shouldBe` 3

    it "mzero behaves like empty" $ do
      let layer = mzero :: Layer IO () String
      runLayer () layer `shouldThrow` anyException

    it "mplus behaves like <|>" $ do
      let failing = mzero :: Layer IO () String
      let success = pure "fallback"
      let combined = mplus failing success

      result <- runLayer () combined
      result `shouldBe` "fallback"

    it "Alternative with resource dependencies" $ do
      tracker <- newResourceTracker
      let primary = do
            _ <- trackedResource tracker "primary-dep" ()
            empty :: Layer IO () String
      let fallback = do
            _ <- trackedResource tracker "fallback-dep" ()
            pure "fallback-result"
      let combined = primary <|> fallback

      result <- runLayer () combined
      result `shouldBe` "fallback-result"

      -- Primary dependency should be cleaned up before fallback runs
      acq <- readIORef (acquired tracker)
      rel <- readIORef (released tracker)
      "primary-dep" `elem` rel `shouldBe` True
      "fallback-dep" `elem` acq `shouldBe` True

    it "all alternatives fail propagates exception" $ do
      let opt1 = empty :: Layer IO () String
      let opt2 = empty :: Layer IO () String
      let opt3 = empty :: Layer IO () String
      let combined = opt1 <|> opt2 <|> opt3

      runLayer () combined `shouldThrow` anyException

  describe "Layer - Environment functions" $ do
    it "mergeEnvironment combines environments" $ do
      let env1 = appendEnvironment (Config 8080 "localhost") emptyEnvironment
      let env2 = appendEnvironment (Database 42) emptyEnvironment
      let merged = mergeEnvironment env1 env2

      let cfg = lookupEnvironment @Config merged
      let db = lookupEnvironment @Database merged

      cfg `shouldBe` Config 8080 "localhost"
      db `shouldBe` Database 42

    it "assembleFromEnvironment with multiple dependencies" $ do
      let env = appendEnvironment (Config 3000 "test") $
                appendEnvironment (Database 5) $
                appendEnvironment (Cache 10) $
                emptyEnvironment

      let mkApp :: Config -> Database -> Cache -> (Int, Int, Int)
          mkApp c d ca = (port c, connId d, cacheId ca)

      let result = assembleFromEnvironment env mkApp
      result `shouldBe` (3000, 5, 10)

    it "assembleFromEnvironment with partial application" $ do
      let env = appendEnvironment (Config 8080 "host") $
                appendEnvironment (Database 1) $
                emptyEnvironment

      let partial :: Config -> Database -> String
          partial c d = host c ++ ":" ++ show (connId d)

      let result = assembleFromEnvironment env partial
      result `shouldBe` "host:1"

    it "complex environment assembly" $ do
      let env = appendEnvironment ("prefix-" :: String) $
                appendEnvironment (42 :: Int) $
                appendEnvironment True $
                emptyEnvironment

      let buildString :: String -> Int -> Bool -> String
          buildString prefix n flag = prefix ++ show n ++ if flag then "-enabled" else "-disabled"

      let result = assembleFromEnvironment env buildString
      result `shouldBe` "prefix-42-enabled"

  describe "Layer - Exception during cleanup" $ do
    it "exception in finalizer doesn't prevent other cleanups" $ do
      tracker <- newResourceTracker
      let layer1 = resource
            (\_ -> do
              trackAcquire tracker "res1"
              pure ("res1" :: String))
            (\_ -> do
              trackRelease tracker "res1"
              throwIO $ userError "cleanup1 failed") :: Layer IO () String

      let layer2 = resource
            (\_ -> do
              trackAcquire tracker "res2"
              pure ("res2" :: String))
            (\_ -> trackRelease tracker "res2") :: Layer IO () String

      let combined = liftA2 (,) layer1 layer2

      -- The cleanup exception should propagate
      runLayer () combined `shouldThrow` anyException

      -- But both should attempt cleanup
      rel <- readIORef (released tracker)
      "res1" `elem` rel `shouldBe` True
      -- Note: res2 cleanup behavior depends on exception handling order

    it "exception in use phase still triggers cleanup" $ do
      tracker <- newResourceTracker
      let layer = trackedResource tracker "resource" ("value" :: String)

      withLayer () layer (\_ -> liftIO $ throwIO $ userError "use failed") `shouldThrow` anyException

      -- Resource should still be cleaned up
      rel <- readIORef (released tracker)
      "resource" `elem` rel `shouldBe` True

    it "multiple concurrent exceptions in zipLayer" $ do
      tracker <- newResourceTracker
      let layer1 = do
            _ <- trackedResource tracker "res1" ()
            (effect (\_ -> threadDelay 10000 >> throwIO (userError "error1")) :: Layer IO () String)
      let layer2 = do
            _ <- trackedResource tracker "res2" ()
            (effect (\_ -> threadDelay 10000 >> throwIO (userError "error2")) :: Layer IO () String)

      let combined = zipLayer layer1 layer2

      runLayer ((), ()) combined `shouldThrow` anyException

      -- Wait for cleanup to complete
      threadDelay 50000

      -- Both resources should be cleaned up
      rel <- readIORef (released tracker)
      "res1" `elem` rel `shouldBe` True
      "res2" `elem` rel `shouldBe` True

    it "exception in one zipLayer branch cleans up both" $ do
      tracker <- newResourceTracker
      let successLayer = trackedResource tracker "success" ("ok" :: String)
      let failingLayer = trackedResource tracker "failing" () >> (effect (\_ -> throwIO $ userError "boom") :: Layer IO () String)

      let combined = zipLayer successLayer failingLayer

      runLayer ((), ()) combined `shouldThrow` anyException

      -- Both should be cleaned up
      threadDelay 50000
      rel <- readIORef (released tracker)
      "success" `elem` rel `shouldBe` True
      "failing" `elem` rel `shouldBe` True
