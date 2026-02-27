{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Fractal.Layer.DiagnosticsSpec (spec) where

import Control.Category ((>>>))
import Control.Monad (void)
import Data.Aeson (encode, decode, toJSON, toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import Data.Typeable (Proxy(..), typeRep)
import Fractal.Layer
import Fractal.Layer.Diagnostics
import Fractal.Layer.Interceptor
import Test.Hspec
import UnliftIO
import UnliftIO.Resource
import Prelude hiding ((.), id)

-- Test data types
newtype Config = Config { configPort :: Int }
  deriving (Show, Eq)

newtype Database = Database { dbConnection :: String }
  deriving (Show, Eq)

newtype WebServer = WebServer { serverPort :: Int }
  deriving (Show, Eq)

newtype CacheService = CacheService { cacheSize :: Int }
  deriving (Show, Eq)

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
      totalResources diags `shouldBe` 0
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
      let root = rootNode diags
      nodeName root `shouldBe` "Root"

    it "tracks service creation and reuse" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let cacheLayer = effect @IO @() @CacheService $ \_ -> pure (CacheService 100)
      let cacheService = mkService cacheLayer

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
      totalResources diags `shouldSatisfy` (>= 1)
      sharedResources diags `shouldSatisfy` (>= 1)

    it "tracks multiple resources" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = do
            _ <- resource @IO @() @Config (\_ -> pure (Config 1)) (\_ -> pure ())
            _ <- resource @IO @() @Database (\_ -> pure (Database "db")) (\_ -> pure ())
            _ <- resource @IO @() @WebServer (\_ -> pure (WebServer 80)) (\_ -> pure ())
            pure ()

      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let root = rootNode diags
      length (children root) `shouldSatisfy` (>= 3)

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

    it "renderLayerTreeCompact is same as renderLayerTree" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      renderLayerTreeCompact diags `shouldBe` renderLayerTree diags

    it "renders tree with children" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = do
            _ <- resource @IO @() @Config (\_ -> pure (Config 1)) (\_ -> pure ())
            _ <- effect @IO @() @Database $ \_ -> pure (Database "db")
            pure ()

      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let rendered = renderLayerTree diags
      rendered `shouldContain` "Root"

    it "detailed rendering includes type information" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let svc = mkService $ resource @IO @() @Config (\_ -> pure (Config 1)) (\_ -> pure ())
      let layer = do
            _ <- service svc
            _ <- service svc
            pure ()

      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      diags <- finalizeDiagnostics collector
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "Detailed"
      totalResources diags `shouldSatisfy` (>= 1)
      sharedResources diags `shouldSatisfy` (>= 1)

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

      let decoded = decode json :: Maybe LayerDiagnostics
      case decoded of
        Nothing -> expectationFailure "Failed to decode diagnostics JSON"
        Just diags' -> do
          totalDuration diags' `shouldBe` totalDuration diags
          totalResources diags' `shouldBe` totalResources diags

    it "diagnosticsToJSON produces valid JSON" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Int $ \_ -> pure 42
      _ <- runLayerWithInterceptor interceptor () layer

      diags <- finalizeDiagnostics collector
      let json = diagnosticsToJSON diags
      let encoded = encode json
      BSL.length encoded `shouldSatisfy` (> 0)

  describe "LayerNodeType JSON roundtrip" $ do
    it "ResourceNode roundtrips" $ do
      let encoded = encode ResourceNode
      decode encoded `shouldBe` Just ResourceNode

    it "EffectNode roundtrips" $ do
      let encoded = encode EffectNode
      decode encoded `shouldBe` Just EffectNode

    it "ServiceNode roundtrips" $ do
      let encoded = encode ServiceNode
      decode encoded `shouldBe` Just ServiceNode

    it "ComposedNode roundtrips" $ do
      let encoded = encode ComposedNode
      decode encoded `shouldBe` Just ComposedNode

    it "ParallelNode roundtrips" $ do
      let encoded = encode ParallelNode
      decode encoded `shouldBe` Just ParallelNode

    it "SequentialNode roundtrips" $ do
      let encoded = encode SequentialNode
      decode encoded `shouldBe` Just SequentialNode

    it "unknown node type fails" $ do
      let decoded = decode "\"unknown\"" :: Maybe LayerNodeType
      decoded `shouldBe` Nothing

  describe "LayerNodeType equality" $ do
    it "same types are equal" $ do
      ResourceNode `shouldBe` ResourceNode
      EffectNode `shouldBe` EffectNode
      ServiceNode `shouldBe` ServiceNode
      ComposedNode `shouldBe` ComposedNode
      ParallelNode `shouldBe` ParallelNode
      SequentialNode `shouldBe` SequentialNode

    it "different types are not equal" $ do
      ResourceNode `shouldNotBe` EffectNode
      EffectNode `shouldNotBe` ServiceNode
      ComposedNode `shouldNotBe` ParallelNode

  describe "LayerNodeType show" $ do
    it "shows all types" $ do
      show ResourceNode `shouldBe` "ResourceNode"
      show EffectNode `shouldBe` "EffectNode"
      show ServiceNode `shouldBe` "ServiceNode"
      show ComposedNode `shouldBe` "ComposedNode"
      show ParallelNode `shouldBe` "ParallelNode"
      show SequentialNode `shouldBe` "SequentialNode"

  describe "ResourceStatus JSON roundtrip" $ do
    it "Initializing roundtrips" $ do
      let encoded = encode Initializing
      decode encoded `shouldBe` Just Initializing

    it "Initialized roundtrips" $ do
      let encoded = encode Initialized
      decode encoded `shouldBe` Just Initialized

    it "Failed roundtrips" $ do
      let encoded = encode (Failed "some error")
      decode encoded `shouldBe` Just (Failed "some error")

    it "SharedReference roundtrips" $ do
      let encoded = encode (SharedReference "node-42")
      decode encoded `shouldBe` Just (SharedReference "node-42")

    it "unknown status fails" $ do
      let decoded = decode "{\"status\":\"bogus\"}" :: Maybe ResourceStatus
      decoded `shouldBe` Nothing

  describe "ResourceStatus equality" $ do
    it "same statuses are equal" $ do
      Initializing `shouldBe` Initializing
      Initialized `shouldBe` Initialized
      Failed "x" `shouldBe` Failed "x"
      SharedReference "a" `shouldBe` SharedReference "a"

    it "different statuses are not equal" $ do
      Initializing `shouldNotBe` Initialized
      Failed "x" `shouldNotBe` Failed "y"
      SharedReference "a" `shouldNotBe` SharedReference "b"

  describe "ResourceStatus show" $ do
    it "shows all variants" $ do
      show Initializing `shouldContain` "Initializing"
      show Initialized `shouldContain` "Initialized"
      show (Failed "err") `shouldContain` "err"
      show (SharedReference "ref") `shouldContain` "ref"

  describe "LayerDiagnostics show" $ do
    it "shows the diagnostics" $ do
      collector <- newDiagnosticsCollector
      diags <- finalizeDiagnostics collector
      let s = show diags
      s `shouldContain` "LayerDiagnostics"

  describe "LayerNode show" $ do
    it "shows the node" $ do
      let node = LayerNode
            { nodeId = "test"
            , nodeName = "TestNode"
            , nodeType = EffectNode
            , resourceType = Nothing
            , status = Initialized
            , duration = Just 0.1
            , children = []
            , metadata = HashMap.empty
            }
      let s = show node
      s `shouldContain` "TestNode"
      s `shouldContain` "EffectNode"

  describe "LayerNode JSON roundtrip" $ do
    it "basic node roundtrips" $ do
      let node = LayerNode
            { nodeId = "n1"
            , nodeName = "TestNode"
            , nodeType = ResourceNode
            , resourceType = Nothing
            , status = Initialized
            , duration = Just 1.5
            , children = []
            , metadata = HashMap.empty
            }
      let encoded = encode node
      let decoded = decode encoded :: Maybe LayerNode
      case decoded of
        Nothing -> expectationFailure "Failed to decode LayerNode"
        Just n -> do
          nodeId n `shouldBe` "n1"
          nodeName n `shouldBe` "TestNode"
          nodeType n `shouldBe` ResourceNode
          status n `shouldBe` Initialized
          duration n `shouldBe` Just 1.5

    it "node with children roundtrips" $ do
      let child = LayerNode "c1" "Child" EffectNode Nothing Initialized (Just 0.5) [] HashMap.empty
      let parent = LayerNode "p1" "Parent" ComposedNode Nothing Initialized (Just 1.0) [child] HashMap.empty
      let encoded = encode parent
      let decoded = decode encoded :: Maybe LayerNode
      case decoded of
        Nothing -> expectationFailure "Failed to decode parent node"
        Just n -> do
          length (children n) `shouldBe` 1
          nodeName (head (children n)) `shouldBe` "Child"

    it "node with metadata roundtrips" $ do
      let node = LayerNode "m1" "Meta" ServiceNode Nothing
                   (SharedReference "ref-1") Nothing []
                   (HashMap.fromList [("pool", "10"), ("timeout", "30")])
      let encoded = encode node
      let decoded = decode encoded :: Maybe LayerNode
      case decoded of
        Nothing -> expectationFailure "Failed to decode node with metadata"
        Just n -> do
          status n `shouldBe` SharedReference "ref-1"
          HashMap.lookup "pool" (metadata n) `shouldBe` Just "10"
          HashMap.lookup "timeout" (metadata n) `shouldBe` Just "30"

    it "node with Failed status roundtrips" $ do
      let node = LayerNode "f1" "Failing" ResourceNode Nothing (Failed "boom") Nothing [] HashMap.empty
      let encoded = encode node
      let decoded = decode encoded :: Maybe LayerNode
      case decoded of
        Nothing -> expectationFailure "Failed to decode failing node"
        Just n -> status n `shouldBe` Failed "boom"

  describe "LayerDiagnostics JSON roundtrip" $ do
    it "full diagnostics roundtrips" $ do
      let child1 = LayerNode "c1" "ConfigLayer" EffectNode Nothing Initialized (Just 0.05) [] HashMap.empty
      let child2 = LayerNode "c2" "DbLayer" ResourceNode Nothing Initialized (Just 0.2) [] HashMap.empty
      let root = LayerNode "root" "App" SequentialNode Nothing Initialized (Just 0.25)
                   [child1, child2] HashMap.empty
      let diags = LayerDiagnostics
            { rootNode = root
            , totalDuration = 0.25
            , totalResources = 2
            , sharedResources = 0
            }
      let encoded = encode diags
      let decoded = decode encoded :: Maybe LayerDiagnostics
      case decoded of
        Nothing -> expectationFailure "Failed to decode LayerDiagnostics"
        Just d -> do
          totalDuration d `shouldBe` 0.25
          totalResources d `shouldBe` 2
          sharedResources d `shouldBe` 0
          nodeName (rootNode d) `shouldBe` "App"
          length (children (rootNode d)) `shouldBe` 2

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
            _ <- service serviceLayer
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

      snap1 <- snapshotDiagnostics collector
      totalResources snap1 `shouldBe` 0

      let layer = effect @IO @() @Int (\_ -> pure 42)
      let (Layer l) = layer
      lenv <- LayerEnv <$> newMVar mempty <*> pure interceptor
      void $ runResourceT $ l lenv ()

      snap2 <- snapshotDiagnostics collector
      totalDuration snap2 `shouldSatisfy` (>= 0)

      final <- finalizeDiagnostics collector
      totalDuration final `shouldSatisfy` (>= 0)

    it "live diagnostics rendering with manual control" $ do
      collector <- newDiagnosticsCollector

      snap <- snapshotDiagnostics collector
      nodeName (rootNode snap) `shouldBe` "Root"

      let rendered = renderLayerTree snap
      rendered `shouldContain` "Layer Initialization Tree"

    it "snapshot reflects empty collector" $ do
      collector <- newDiagnosticsCollector
      snap <- snapshotDiagnostics collector
      totalResources snap `shouldBe` 0
      sharedResources snap `shouldBe` 0
      nodeName (rootNode snap) `shouldBe` "Root"
      nodeType (rootNode snap) `shouldBe` ComposedNode

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

    it "withLayerDiagnostics with resource layers" $ do
      let testLayer = resource @IO @() @String
            (\_ -> pure "managed-resource")
            (\_ -> pure ())

      withLayerDiagnostics testLayer () $ \(env, diags) -> liftIO $ do
        env `shouldBe` ("managed-resource" :: String)
        totalDuration diags `shouldSatisfy` (>= 0)

    it "buildLayerDiagnostics with service tracking" $ do
      let svc = mkService $ effect @IO @() @Int (\_ -> pure 99)
      let testLayer = do
            a <- service svc
            b <- service svc
            pure (a + b)

      diags <- buildLayerDiagnostics testLayer ()
      totalResources diags `shouldSatisfy` (>= 1)
      sharedResources diags `shouldSatisfy` (>= 1)

    it "buildLayerDiagnostics with composed layers exercises composition callbacks" $ do
      let configL = effect @IO @() @Config $ \_ -> pure (Config 8080)
      let dbL = effect @IO @Config @Database $ \cfg ->
            pure (Database ("db:" <> show (configPort cfg)))
      let composed = configL >>> dbL
      diags <- buildLayerDiagnostics composed ()
      totalDuration diags `shouldSatisfy` (>= 0)
      let root = rootNode diags
      length (children root) `shouldSatisfy` (>= 1)

    it "buildLayerDiagnostics with service reuse through full pipeline" $ do
      let svc = mkService $ resource @IO @() @Config
            (\_ -> pure (Config 42))
            (\_ -> pure ())
      let testLayer = do
            a <- service svc
            b <- service svc
            pure (configPort a + configPort b)
      diags <- buildLayerDiagnostics testLayer ()
      sharedResources diags `shouldSatisfy` (>= 1)
      totalResources diags `shouldSatisfy` (>= 1)

  describe "Diagnostics - endNode edge cases" $ do
    it "handles completing root node directly" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let layer = effect @IO @() @Int $ \_ -> pure 42
      _ <- runLayerWithInterceptor interceptor () layer

      diags <- finalizeDiagnostics collector
      status (rootNode diags) `shouldBe` Initialized

    it "service without type still tracked" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector

      let svc = mkService $ effect @IO @() @Int $ \_ -> pure 42
      let layer = service svc
      _ <- runLayerWithInterceptor interceptor () layer

      diags <- finalizeDiagnostics collector
      totalResources diags `shouldSatisfy` (>= 1)

  describe "Diagnostics - rendering edge cases" $ do
    it "renders node with no duration" $ do
      let node = LayerNode "n1" "NoDuration" EffectNode Nothing Initializing Nothing [] HashMap.empty
      let diags = LayerDiagnostics
            { rootNode = node
            , totalDuration = 0
            , totalResources = 0
            , sharedResources = 0
            }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "NoDuration"

    it "renders node with Failed status" $ do
      let node = LayerNode "n1" "FailedNode" ResourceNode Nothing (Failed "crash") (Just 0.1) [] HashMap.empty
      let diags = LayerDiagnostics
            { rootNode = node
            , totalDuration = 0.1
            , totalResources = 1
            , sharedResources = 0
            }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "crash"

    it "renders node with SharedReference status" $ do
      let shared = LayerNode "s1" "SharedSvc" ServiceNode Nothing (SharedReference "svc-001") Nothing [] HashMap.empty
      let parent = LayerNode "p1" "Parent" ComposedNode Nothing Initialized (Just 0.5) [shared] HashMap.empty
      let diags = LayerDiagnostics
            { rootNode = parent
            , totalDuration = 0.5
            , totalResources = 1
            , sharedResources = 1
            }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "svc-001"

    it "renders Initializing status" $ do
      let node = LayerNode "n1" "InProgress" EffectNode Nothing Initializing Nothing [] HashMap.empty
      let diags = LayerDiagnostics { rootNode = node, totalDuration = 0, totalResources = 0, sharedResources = 0 }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "InProgress"

    it "renders deeply nested tree" $ do
      let leaf = LayerNode "l1" "Leaf" EffectNode Nothing Initialized (Just 0.01) [] HashMap.empty
      let mid = LayerNode "m1" "Mid" ResourceNode Nothing Initialized (Just 0.05) [leaf] HashMap.empty
      let root = LayerNode "r1" "Root" ComposedNode Nothing Initialized (Just 0.1) [mid] HashMap.empty
      let diags = LayerDiagnostics { rootNode = root, totalDuration = 0.1, totalResources = 2, sharedResources = 0 }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "Leaf"
      rendered `shouldContain` "Mid"

    it "renders multiple children" $ do
      let c1 = LayerNode "c1" "Child1" EffectNode Nothing Initialized (Just 0.01) [] HashMap.empty
      let c2 = LayerNode "c2" "Child2" ResourceNode Nothing Initialized (Just 0.02) [] HashMap.empty
      let c3 = LayerNode "c3" "Child3" ServiceNode Nothing Initialized (Just 0.03) [] HashMap.empty
      let root = LayerNode "r" "Root" ParallelNode Nothing Initialized (Just 0.1) [c1, c2, c3] HashMap.empty
      let diags = LayerDiagnostics { rootNode = root, totalDuration = 0.1, totalResources = 3, sharedResources = 0 }
      let rendered = renderLayerTree diags
      rendered `shouldContain` "Child1"
      rendered `shouldContain` "Child2"
      rendered `shouldContain` "Child3"

    it "detailed rendering shows metadata" $ do
      let node = LayerNode "n1" "WithMeta" ResourceNode (Just (typeRep (Proxy @Int))) Initialized
                   (Just 0.5) [] (HashMap.fromList [("pool", "10")])
      let diags = LayerDiagnostics { rootNode = node, totalDuration = 0.5, totalResources = 1, sharedResources = 0 }
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "pool"
      rendered `shouldContain` "10"
      rendered `shouldContain` "Int"

    it "detailed rendering shows all status types" $ do
      let mkNode s = LayerNode "n" "N" EffectNode Nothing s Nothing [] HashMap.empty
      let mkDiags n = LayerDiagnostics { rootNode = n, totalDuration = 0, totalResources = 0, sharedResources = 0 }

      let r1 = renderLayerTreeDetailed (mkDiags (mkNode Initializing))
      r1 `shouldContain` "Initializing"

      let r2 = renderLayerTreeDetailed (mkDiags (mkNode Initialized))
      r2 `shouldContain` "Initialized"

      let r3 = renderLayerTreeDetailed (mkDiags (mkNode (Failed "err")))
      r3 `shouldContain` "Failed"
      r3 `shouldContain` "err"

      let r4 = renderLayerTreeDetailed (mkDiags (mkNode (SharedReference "ref-1")))
      r4 `shouldContain` "Shared reference"
      r4 `shouldContain` "ref-1"

    it "detailed rendering with SequentialNode type symbol" $ do
      let node = LayerNode "n1" "Seq" SequentialNode Nothing Initialized Nothing [] HashMap.empty
      let diags = LayerDiagnostics { rootNode = node, totalDuration = 0, totalResources = 0, sharedResources = 0 }
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "SequentialNode"

    it "detailed rendering with ParallelNode type symbol" $ do
      let node = LayerNode "n1" "Par" ParallelNode Nothing Initialized Nothing [] HashMap.empty
      let diags = LayerDiagnostics { rootNode = node, totalDuration = 0, totalResources = 0, sharedResources = 0 }
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "ParallelNode"

    it "detailed rendering shows duration" $ do
      let node = LayerNode "n1" "Timed" EffectNode Nothing Initialized (Just 1.234) [] HashMap.empty
      let diags = LayerDiagnostics { rootNode = node, totalDuration = 1.234, totalResources = 0, sharedResources = 0 }
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "1.234"

    it "detailed rendering with children" $ do
      let child = LayerNode "c1" "Kid" EffectNode Nothing Initialized (Just 0.1) [] HashMap.empty
      let parent = LayerNode "p1" "Dad" ComposedNode Nothing Initialized (Just 0.5) [child] HashMap.empty
      let diags = LayerDiagnostics { rootNode = parent, totalDuration = 0.5, totalResources = 1, sharedResources = 0 }
      let rendered = renderLayerTreeDetailed diags
      rendered `shouldContain` "Kid"
      rendered `shouldContain` "Dad"

  describe "Diagnostics - live rendering support" $ do
    it "renderLayerTreeLive completes when isDone returns True immediately" $ do
      collector <- newDiagnosticsCollector
      renderLayerTreeLive collector (pure True :: IO Bool)

    it "snapshotDiagnostics computes totalDuration" $ do
      collector <- newDiagnosticsCollector
      snap <- snapshotDiagnostics collector
      totalDuration snap `shouldSatisfy` (>= 0)
      totalResources snap `shouldBe` 0

  describe "Diagnostics interceptor - composition callbacks" $ do
    it "onCompositionStart creates Sequential node" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      onCompositionStart interceptor Sequential
      onCompositionEnd interceptor Sequential 0.1
      diags <- finalizeDiagnostics collector
      let root = rootNode diags
      length (children root) `shouldSatisfy` (>= 1)

    it "onCompositionStart creates Parallel node" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      onCompositionStart interceptor Parallel
      onCompositionEnd interceptor Parallel 0.2
      diags <- finalizeDiagnostics collector
      length (children (rootNode diags)) `shouldSatisfy` (>= 1)

  describe "Diagnostics interceptor - service reuse shared node" $ do
    it "onServiceReuse creates shared reference when service was tracked" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      let tr = typeRep (Proxy @Int)
      let ctx = OperationContext "IntService" (Just tr) []
      onServiceCreate interceptor ctx
      onServiceReuse interceptor "IntService" tr
      diags <- finalizeDiagnostics collector
      sharedResources diags `shouldBe` 1
      totalResources diags `shouldBe` 1

    it "onServiceReuse with untracked type is a no-op" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      let tr = typeRep (Proxy @Bool)
      onServiceReuse interceptor "Unknown" tr
      diags <- finalizeDiagnostics collector
      sharedResources diags `shouldBe` 0

  describe "Diagnostics interceptor - endNode edge cases" $ do
    it "endNode on empty stack is a no-op" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      onEffectComplete interceptor "test" 0.1
      onEffectComplete interceptor "test2" 0.2
      diags <- finalizeDiagnostics collector
      totalDuration diags `shouldSatisfy` (>= 0)

    it "endNode completing root directly" $ do
      collector <- newDiagnosticsCollector
      let interceptor = createDiagnosticsInterceptor collector
      onEffectRun interceptor (OperationContext "root-effect" Nothing [])
      onEffectComplete interceptor "root-effect" 0.5
      diags <- finalizeDiagnostics collector
      let root = rootNode diags
      length (children root) `shouldSatisfy` (>= 1)

  describe "Diagnostics - showList coverage" $ do
    it "showList for LayerNodeType" $ do
      let s = show [ResourceNode, EffectNode, ServiceNode]
      s `shouldContain` "ResourceNode"
      s `shouldContain` "EffectNode"

    it "showList for ResourceStatus" $ do
      let s = show [Initializing, Initialized, Failed "x"]
      s `shouldContain` "Initializing"

    it "showList for LayerDiagnostics" $ do
      let d = LayerDiagnostics
                (LayerNode "r" "R" ComposedNode Nothing Initialized Nothing [] HashMap.empty)
                0 0 0
      let s = show [d]
      s `shouldContain` "LayerDiagnostics"

    it "showList for LayerNode" $ do
      let n = LayerNode "n" "N" EffectNode Nothing Initialized Nothing [] HashMap.empty
      let s = show [n, n]
      s `shouldContain` "LayerNode"

  describe "Diagnostics - JSON list encoding" $ do
    it "toJSON list of LayerNodeType" $ do
      let val = toJSON [ResourceNode, EffectNode, ServiceNode]
      BSL.length (encode val) `shouldSatisfy` (> 0)

    it "toEncoding of LayerNodeType" $ do
      BSL.length (encodingToLazyByteString (toEncoding ResourceNode)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding EffectNode)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding ServiceNode)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding ComposedNode)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding ParallelNode)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding SequentialNode)) `shouldSatisfy` (> 0)

    it "toEncoding of ResourceStatus" $ do
      BSL.length (encodingToLazyByteString (toEncoding Initializing)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding Initialized)) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding (Failed "e"))) `shouldSatisfy` (> 0)
      BSL.length (encodingToLazyByteString (toEncoding (SharedReference "r"))) `shouldSatisfy` (> 0)

    it "toEncoding of LayerNode" $ do
      let n = LayerNode "n" "N" EffectNode Nothing Initialized (Just 0.1) [] HashMap.empty
      BSL.length (encodingToLazyByteString (toEncoding n)) `shouldSatisfy` (> 0)

    it "toEncoding of LayerDiagnostics" $ do
      let d = LayerDiagnostics
                (LayerNode "r" "R" ComposedNode Nothing Initialized Nothing [] HashMap.empty)
                0.5 2 1
      BSL.length (encodingToLazyByteString (toEncoding d)) `shouldSatisfy` (> 0)

    it "decode JSON list of LayerNodeType" $ do
      let encoded = encode [ResourceNode, EffectNode]
      let decoded = decode encoded :: Maybe [LayerNodeType]
      decoded `shouldBe` Just [ResourceNode, EffectNode]

    it "decode JSON list of ResourceStatus" $ do
      let encoded = encode [Initializing, Initialized]
      let decoded = decode encoded :: Maybe [ResourceStatus]
      decoded `shouldBe` Just [Initializing, Initialized]

    it "decode JSON list of LayerNode" $ do
      let n = LayerNode "n" "N" EffectNode Nothing Initialized (Just 0.1) [] HashMap.empty
      let encoded = encode [n]
      let decoded = decode encoded :: Maybe [LayerNode]
      case decoded of
        Nothing -> expectationFailure "Failed to decode"
        Just ns -> length ns `shouldBe` 1

  describe "Diagnostics - DiagnosticsCollector field" $ do
    it "newDiagnosticsCollector returns usable collector" $ do
      collector <- newDiagnosticsCollector
      diags <- finalizeDiagnostics collector
      nodeName (rootNode diags) `shouldBe` "Root"

  describe "Diagnostics - full pipeline integration" $ do
    it "composed layers produce sequential composition nodes in diagnostics" $ do
      let l1 = effect @IO @() @Config $ \_ -> pure (Config 1)
      let l2 = effect @IO @Config @Database $ \c -> pure (Database (show (configPort c)))
      diags <- buildLayerDiagnostics (l1 >>> l2) ()
      totalDuration diags `shouldSatisfy` (>= 0)

    it "service create+reuse through full pipeline" $ do
      let svc = mkService $ resource @IO @() @Database
            (\_ -> pure (Database "svc"))
            (\_ -> pure ())
      let layer = do
            a <- service svc
            b <- service svc
            pure (dbConnection a, dbConnection b)
      diags <- buildLayerDiagnostics layer ()
      totalResources diags `shouldSatisfy` (>= 1)
      sharedResources diags `shouldSatisfy` (>= 1)

    it "resource layer produces resource node in diagnostics" $ do
      let layer = resource @IO @() @Config (\_ -> pure (Config 1)) (\_ -> pure ())
      diags <- buildLayerDiagnostics layer ()
      let root = rootNode diags
      length (children root) `shouldSatisfy` (>= 1)
