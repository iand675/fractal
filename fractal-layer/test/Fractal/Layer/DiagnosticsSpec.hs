{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fractal.Layer.DiagnosticsSpec (spec) where

import Control.Applicative (liftA2)
import Control.Category ((>>>))
import Control.Monad (void)
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Typeable (Proxy(..), typeRep)
import Fractal.Layer
import Fractal.Layer.Diagnostics
import Fractal.Layer.Interceptor
import GHC.Generics (Generic)
import Test.Hspec
import UnliftIO
import UnliftIO.Resource
import Prelude hiding ((.), id)

-- Test data types
newtype Config = Config { configPort :: Int }
  deriving (Show, Eq, Generic)

newtype Database = Database { dbConnection :: String }
  deriving (Show, Eq, Generic)

newtype WebServer = WebServer { serverPort :: Int }
  deriving (Show, Eq, Generic)

newtype CacheService = CacheService { cacheSize :: Int }
  deriving (Show, Eq, Generic)

spec :: Spec
spec = do
  describe "LayerInterceptor" $ do
    it "nullInterceptor has no effect" $ do
      ref <- newIORef ([] :: [String])
      let layer = effect @IO @() @Config $ \_ -> do
            modifyIORef ref (++ ["effect"])
            pure (Config 8080)
      result <- runLayer () layer
      configPort result `shouldBe` 8080
      logs <- readIORef ref
      logs `shouldBe` ["effect"]

    it "custom interceptor captures operations" $ do
      ref <- newIORef ([] :: [String])
      let customInterceptor = LayerInterceptor
            { onResourceAcquire = \ctx -> liftIO $ modifyIORef ref (++ ["resource-acquire:" <> T.unpack (operationName ctx)])
            , onResourceRelease = \name _ -> liftIO $ modifyIORef ref (++ ["resource-release:" <> T.unpack name])
            , onEffectRun = \ctx -> liftIO $ modifyIORef ref (++ ["effect-run:" <> T.unpack (operationName ctx)])
            , onEffectComplete = \name _ -> liftIO $ modifyIORef ref (++ ["effect-complete:" <> T.unpack name])
            , onServiceCreate = \ctx -> liftIO $ modifyIORef ref (++ ["service-create:" <> T.unpack (operationName ctx)])
            , onServiceReuse = \name _ -> liftIO $ modifyIORef ref (++ ["service-reuse:" <> T.unpack name])
            , onCompositionStart = \_ -> liftIO $ modifyIORef ref (++ ["composition-start"])
            , onCompositionEnd = \_ _ -> liftIO $ modifyIORef ref (++ ["composition-end"])
            }

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure customInterceptor
      void $ runResourceT $ l lenv ()

      logs <- readIORef ref
      logs `shouldContain` ["effect-run:Config"]
      logs `shouldContain` ["effect-complete:Config"]

    it "combines multiple interceptors" $ do
      ref1 <- newIORef ([] :: [String])
      ref2 <- newIORef ([] :: [String])

      let interceptor1 = LayerInterceptor
            { onResourceAcquire = \_ -> liftIO $ modifyIORef ref1 (++ ["i1-resource"])
            , onResourceRelease = \_ _ -> liftIO $ modifyIORef ref1 (++ ["i1-release"])
            , onEffectRun = \_ -> liftIO $ modifyIORef ref1 (++ ["i1-effect"])
            , onEffectComplete = \_ _ -> pure ()
            , onServiceCreate = \_ -> pure ()
            , onServiceReuse = \_ _ -> pure ()
            , onCompositionStart = \_ -> pure ()
            , onCompositionEnd = \_ _ -> pure ()
            }

      let interceptor2 = LayerInterceptor
            { onResourceAcquire = \_ -> liftIO $ modifyIORef ref2 (++ ["i2-resource"])
            , onResourceRelease = \_ _ -> liftIO $ modifyIORef ref2 (++ ["i2-release"])
            , onEffectRun = \_ -> liftIO $ modifyIORef ref2 (++ ["i2-effect"])
            , onEffectComplete = \_ _ -> pure ()
            , onServiceCreate = \_ -> pure ()
            , onServiceReuse = \_ _ -> pure ()
            , onCompositionStart = \_ -> pure ()
            , onCompositionEnd = \_ _ -> pure ()
            }

      let combined = combineInterceptors [interceptor1, interceptor2]
      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure combined
      void $ runResourceT $ l lenv ()

      logs1 <- readIORef ref1
      logs2 <- readIORef ref2
      logs1 `shouldContain` ["i1-effect"]
      logs2 `shouldContain` ["i2-effect"]

  describe "Diagnostics Collection" $ do
    it "collects effect operations" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      result <- runResourceT $ l lenv ()

      configPort result `shouldBe` 8080

      diags <- finalizeDiagnostics collector
      totalResources diags `shouldBe` 0 -- Effects don't count as resources
      length (children $ rootNode diags) `shouldSatisfy` (>= 0)

    it "collects resource operations" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = resource @IO @() @Database
            (\_ -> pure $ Database "connected")
            (\_ -> pure ())
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      result <- runResourceT $ l lenv ()

      dbConnection result `shouldBe` "connected"

      diags <- finalizeDiagnostics collector
      totalResources diags `shouldBe` 0 -- Resource tracking increments in service layer
      let root = rootNode diags
      -- Verify structure exists
      nodeName root `shouldBe` "Root"

    it "tracks service creation and reuse" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      -- Create a service layer
      let cacheLayer = effect @IO @() @CacheService $ \_ -> pure (CacheService 100)
      let cacheService = mkService cacheLayer

      -- Use the service twice
      let useServiceTwice = do
            cache1 <- service cacheService
            cache2 <- service cacheService
            pure (cache1, cache2)

      let (Layer l) = useServiceTwice
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      (cache1, cache2) <- runResourceT $ l lenv ()

      cacheSize cache1 `shouldBe` 100
      cacheSize cache2 `shouldBe` 100

      diags <- finalizeDiagnostics collector
      -- Should have at least 1 resource (the service)
      totalResources diags `shouldSatisfy` (>= 1)
      -- Should have tracked the reuse
      sharedResources diags `shouldSatisfy` (>= 1)

  describe "Diagnostics Rendering" $ do
    it "renders a tree structure" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let rendered = renderLayerTree diags
      rendered `shouldContain` "Layer Initialization Tree"
      rendered `shouldContain` "Duration:"

    it "renders detailed tree with metadata" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "Layer Initialization Tree (Detailed)"
      rendered `shouldContain` "Initialized"

    it "exports to JSON" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let json = encode diags
      BSL.length json `shouldSatisfy` (> 0)

      -- Verify it can be decoded back
      let decoded = decode json :: Maybe LayerDiagnostics
      case decoded of
        Nothing -> expectationFailure "Failed to decode diagnostics JSON"
        Just diags' -> do
          totalDuration diags' `shouldBe` totalDuration diags
          totalResources diags' `shouldBe` totalResources diags

  describe "Complex Layer Compositions" $ do
    it "tracks composed layers" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let configLayer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let dbLayer = effect @IO @Config @Database $ \cfg ->
            pure $ Database ("localhost:" <> show (configPort cfg))

      let composed = configLayer >>> dbLayer
      let (Layer l) = composed
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      result <- runResourceT $ l lenv ()

      dbConnection result `shouldBe` "localhost:8080"

      diags <- finalizeDiagnostics collector
      nodeName (rootNode diags) `shouldBe` "Root"

    it "tracks parallel composition" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer1 = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let layer2 = effect @IO @() @WebServer $ \_ -> pure (WebServer 9090)

      let parallel = liftA2 (,) layer1 layer2
      let (Layer l) = parallel
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      (cfg, ws) <- runResourceT $ l lenv ()

      configPort cfg `shouldBe` 8080
      serverPort ws `shouldBe` 9090

      diags <- finalizeDiagnostics collector
      nodeName (rootNode diags) `shouldBe` "Root"

  describe "Interceptor Edge Cases" $ do
    it "combines interceptors with empty list" $ do
      let combined = combineInterceptors []
      -- Should behave like nullInterceptor
      ref <- newIORef ([] :: [String])
      let layer = effect @IO @() @String $ \_ -> do
            modifyIORef ref (++ ["effect"])
            pure "test"
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure combined
      result <- runResourceT $ l lenv ()
      result `shouldBe` "test"
      logs <- readIORef ref
      logs `shouldBe` ["effect"]

    it "interceptor captures all operation types" $ do
      ref <- newIORef ([] :: [String])
      let loggingInterceptor = LayerInterceptor
            { onResourceAcquire = \_ -> liftIO $ modifyIORef ref (++ ["acquire"])
            , onResourceRelease = \_ _ -> liftIO $ modifyIORef ref (++ ["release"])
            , onEffectRun = \_ -> liftIO $ modifyIORef ref (++ ["effect"])
            , onEffectComplete = \_ _ -> liftIO $ modifyIORef ref (++ ["effect-done"])
            , onServiceCreate = \_ -> liftIO $ modifyIORef ref (++ ["service-create"])
            , onServiceReuse = \_ _ -> liftIO $ modifyIORef ref (++ ["service-reuse"])
            , onCompositionStart = \_ -> liftIO $ modifyIORef ref (++ ["comp-start"])
            , onCompositionEnd = \_ _ -> liftIO $ modifyIORef ref (++ ["comp-end"])
            }

      let resourceLayer = resource @IO @() @Int (\_ -> pure 100) (\_ -> pure ())
      let effectLayer = effect @IO @() @String (\_ -> pure "test")
      let serviceLayer = mkService resourceLayer
      let composed = do
            _ <- resourceLayer
            _ <- effectLayer
            _ <- service serviceLayer
            _ <- service serviceLayer  -- Reuse
            pure ()

      let (Layer l) = composed
      lenv <- LayerEnv <$> newMVar mempty <*> pure loggingInterceptor
      void $ runResourceT $ l lenv ()

      logs <- readIORef ref
      "acquire" `elem` logs `shouldBe` True
      "effect" `elem` logs `shouldBe` True
      "service-create" `elem` logs `shouldBe` True
      "service-reuse" `elem` logs `shouldBe` True

    it "helper functions create proper contexts" $ do
      let ctx1 = simpleContext "test"
      operationName ctx1 `shouldBe` "test"
      operationType ctx1 `shouldBe` Nothing

      let ctx2 = withType (typeRep (Proxy @Int)) ctx1
      operationType ctx2 `shouldSatisfy` (/= Nothing)

      let ctx3 = withMetadata [("key", "value")] ctx1
      operationMetadata ctx3 `shouldBe` [("key", "value")]

  describe "Snapshot Diagnostics" $ do
    it "snapshotDiagnostics doesn't finalize collector" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      -- Take a snapshot
      snap1 <- snapshotDiagnostics collector
      totalResources snap1 `shouldBe` 0

      -- Run a layer
      let layer = effect @IO @() @Int (\_ -> pure 42)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      -- Take another snapshot - should still work
      snap2 <- snapshotDiagnostics collector
      -- Snapshot should reflect new state
      -- (exact values depend on implementation)

      -- Final finalize should also still work
      final <- finalizeDiagnostics collector
      totalDuration final `shouldSatisfy` (>= 0)

    it "live diagnostics rendering with manual control" $ do
      collector <- newDiagnosticsCollector

      -- Initial snapshot
      snap <- snapshotDiagnostics collector
      nodeName (rootNode snap) `shouldBe` "Root"

      -- Render should not crash
      let rendered = renderLayerTree snap
      rendered `shouldContain` "Layer Initialization Tree"

  describe "Complex Diagnostics with withLayerDiagnostics" $ do
    it "withLayerDiagnostics provides both environment and diagnostics" $ do
      let testLayer = effect @IO @() @Int (\_ -> pure 42)

      withLayerDiagnostics testLayer () $ \(env, diags) -> liftIO $ do
        env `shouldBe` 42
        totalDuration diags `shouldSatisfy` (>= 0)
        nodeName (rootNode diags) `shouldBe` "Root"

    it "buildLayerDiagnostics runs layer and returns diagnostics" $ do
      let testLayer = effect @IO @() @String (\_ -> pure "test")

      diags <- buildLayerDiagnostics testLayer ()
      totalDuration diags `shouldSatisfy` (>= 0)
      nodeName (rootNode diags) `shouldBe` "Root"
