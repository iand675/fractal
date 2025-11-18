# Layer Diagnostics Integration Design

## Overview

The `Fractal.Layer.Diagnostics` module provides visualization and debugging capabilities for layer initialization. This document outlines how to integrate it with the existing `Fractal.Layer` module.

## Architecture

### Core Concept

The diagnostics system tracks layer initialization in real-time by instrumenting the `Layer` type. It builds a tree structure that shows:

- **Sequential composition** (>>> operator)
- **Parallel composition** (&&& operator)
- **Service caching** (shared resources)
- **Resource allocation** timing
- **Failure points** and error messages

### Data Flow

```
Layer Execution
    |
    v
DiagnosticsCollector (IORef LayerNode)
    |
    v
LayerDiagnostics (final tree)
    |
    +---> JSON export
    +---> Terminal rendering
```

## Integration Points

### 1. LayerEnv Enhancement

Extend the `LayerEnv` to include diagnostics collection:

```haskell
data LayerEnv = LayerEnv
  { serviceStates :: !(MVar (HashMap TypeRep (ServiceState Any)))
  , diagnosticsEnabled :: !Bool
  , diagnosticsCollector :: !(Maybe (IORef DiagnosticsState))
  }

data DiagnosticsState = DiagnosticsState
  { currentNode :: LayerNode
  , nodeStack :: [LayerNode]
  , startTime :: UTCTime
  , nextNodeId :: Int
  }
```

### 2. Layer Constructor Instrumentation

Modify layer constructors to record initialization:

```haskell
resource ::
  MonadUnliftIO m =>
  (deps -> m env) ->
  (env -> m ()) ->
  Layer m deps env
resource acq rel = Layer $ \lenv deps -> do
  -- Start diagnostics tracking
  startTime <- liftIO getCurrentTime
  nodeId <- recordNodeStart lenv ResourceNode "Resource"

  -- Original logic
  (_, env) <- allocateU (lift $ acq deps) (lift . rel)

  -- End diagnostics tracking
  endTime <- liftIO getCurrentTime
  recordNodeEnd lenv nodeId (diffUTCTime endTime startTime) Initialized

  pure env
```

### 3. Service Caching Detection

Track when services are shared vs. freshly initialized:

```haskell
getOrCreateCachedService :: forall m deps env. (MonadUnliftIO m, Typeable env) =>
  Service m deps env -> Layer m deps env
getOrCreateCachedService (Service m) = Layer $ \lenv env -> do
  let rep = typeRep (Proxy @env)
  states <- liftIO $ readMVar lenv.serviceStates

  case HashMap.lookup rep states of
    Just (Initialized x) -> do
      -- Record shared reference
      recordSharedService lenv rep
      pure $ unsafeCoerce x
    Nothing -> do
      -- Record new service initialization
      nodeId <- recordNodeStart lenv ServiceNode (show rep)
      -- ... initialize service ...
      recordNodeEnd lenv nodeId duration Initialized
      -- ...
```

### 4. Composition Tracking

Track sequential and parallel composition:

```haskell
composeLayer ::
  Monad m =>
  Layer m a b ->
  Layer m b c ->
  Layer m a c
composeLayer (Layer upper) (Layer lower) = Layer $ \lenv a -> do
  nodeId <- recordNodeStart lenv SequentialNode "Sequential"
  b <- upper lenv a
  c <- lower lenv b
  recordNodeEnd lenv nodeId Nothing Initialized
  pure c

zipLayer ::
  MonadUnliftIO m =>
  Layer m d1 o1 ->
  Layer m d2 o2 ->
  Layer m (d1, d2) (o1, o2)
zipLayer (Layer l1) (Layer l2) = Layer $ \lenv (d1, d2) -> do
  nodeId <- recordNodeStart lenv ParallelNode "Parallel"
  -- ... parallel execution ...
  recordNodeEnd lenv nodeId (Just totalDuration) Initialized
  pure (envA, envB)
```

## API Design

### Running with Diagnostics

```haskell
-- Enable diagnostics for a layer execution
withLayerDiagnostics ::
  MonadUnliftIO m =>
  deps ->
  Layer m deps env ->
  (env -> ResourceT m (r, LayerDiagnostics)) ->
  m (r, LayerDiagnostics)
withLayerDiagnostics deps (Layer l) useEnv = runResourceT $ do
  -- Initialize diagnostics collector
  startTime <- liftIO getCurrentTime
  diagRef <- liftIO $ newIORef emptyDiagnosticsState

  -- Create enhanced LayerEnv
  lenv <- LayerEnv
    <$> newMVar HashMap.empty
    <*> pure True
    <*> pure (Just diagRef)

  -- Run layer
  env <- l lenv deps

  -- Collect final diagnostics
  endTime <- liftIO getCurrentTime
  diagState <- liftIO $ readIORef diagRef
  let diags = finalizeDiagnostics diagState startTime endTime

  -- Run user action
  result <- useEnv env

  pure (result, diags)
```

### Helper Functions

```haskell
-- Record the start of a node
recordNodeStart :: MonadIO m => LayerEnv -> LayerNodeType -> Text -> m Text
recordNodeStart lenv nodeType name = liftIO $ do
  case diagnosticsCollector lenv of
    Nothing -> pure ""
    Just ref -> do
      state <- readIORef ref
      let nodeId = "node-" <> T.pack (show $ nextNodeId state)
      startTime <- getCurrentTime
      let newNode = LayerNode
            { nodeId = nodeId
            , nodeName = name
            , nodeType = nodeType
            , resourceType = Nothing
            , status = Initializing
            , duration = Nothing
            , children = []
            , metadata = HashMap.empty
            }
      -- Push node onto stack
      modifyIORef' ref $ \s -> s
        { nodeStack = newNode : nodeStack s
        , nextNodeId = nextNodeId s + 1
        }
      pure nodeId

-- Record the end of a node
recordNodeEnd :: MonadIO m => LayerEnv -> Text -> Maybe NominalDiffTime -> ResourceStatus -> m ()
recordNodeEnd lenv nodeId maybeDuration status = liftIO $ do
  case diagnosticsCollector lenv of
    Nothing -> pure ()
    Just ref -> do
      modifyIORef' ref $ \state ->
        let (completed:parent:rest) = nodeStack state
            updatedNode = completed
              { status = status
              , duration = realToFrac <$> maybeDuration
              }
            updatedParent = parent
              { children = children parent ++ [updatedNode]
              }
        in state { nodeStack = updatedParent : rest }

-- Record a shared service reference
recordSharedService :: MonadIO m => LayerEnv -> TypeRep -> m ()
recordSharedService lenv typeRep = liftIO $ do
  case diagnosticsCollector lenv of
    Nothing -> pure ()
    Just ref -> do
      -- Create a node with SharedReference status
      -- pointing to the original service node
      pure ()
```

## Output Formats

### Terminal ASCII Art

```
Layer Initialization Tree
=========================

Total Duration: 0.35s
Total Resources: 5
Shared Resources: 1

ApplicationLayer
├── ConfigLayer (0.05s)
└── ParallelServices (0.3s)
    ├── DatabaseLayer (0.2s)
    │   └── ConnectionPool [SERVICE: svc-001] (0.15s)
    └── WebServerLayer (0.1s)
        └── MetricsCollector [SHARED: svc-001]
```

### JSON Output

```json
{
  "root": {
    "id": "root",
    "name": "ApplicationLayer",
    "type": "sequential",
    "status": {"status": "initialized"},
    "duration": 0.35,
    "children": [
      {
        "id": "node-1",
        "name": "ConfigLayer",
        "type": "effect",
        "resourceType": "Config",
        "status": {"status": "initialized"},
        "duration": 0.05,
        "children": [],
        "metadata": {"source": "environment"}
      },
      {
        "id": "node-2",
        "name": "ParallelServices",
        "type": "parallel",
        "status": {"status": "initialized"},
        "duration": 0.3,
        "children": [
          {
            "id": "node-3",
            "name": "DatabaseLayer",
            "type": "resource",
            "status": {"status": "initialized"},
            "duration": 0.2,
            "children": [
              {
                "id": "svc-001",
                "name": "ConnectionPool",
                "type": "service",
                "status": {"status": "initialized"},
                "duration": 0.15,
                "children": [],
                "metadata": {"poolSize": "10"}
              }
            ]
          },
          {
            "id": "node-4",
            "name": "WebServerLayer",
            "type": "resource",
            "status": {"status": "initialized"},
            "duration": 0.1,
            "children": [
              {
                "id": "node-5",
                "name": "MetricsCollector",
                "type": "service",
                "status": {
                  "status": "shared",
                  "reference": "svc-001"
                },
                "children": []
              }
            ]
          }
        ]
      }
    ]
  },
  "totalDuration": 0.35,
  "totalResources": 5,
  "sharedResources": 1
}
```

## Usage Examples

### Basic Usage

```haskell
import Fractal.Layer
import Fractal.Layer.Diagnostics
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = withLayerDiagnostics () appLayer $ \(env, diags) -> do
  -- Print tree to terminal
  putStrLn $ renderLayerTree diags

  -- Export to JSON
  BSL.writeFile "diagnostics.json" (encode diags)

  -- Use environment
  let (cfg, db, ws) = env
  runApp cfg db ws
```

### Debugging Slow Initialization

```haskell
debugLayerPerformance :: Layer IO () env -> IO ()
debugLayerPerformance layer = withLayerDiagnostics () layer $ \(_, diags) -> do
  putStrLn $ renderLayerTreeDetailed diags

  -- Find slow nodes
  let slowNodes = filter (\n -> maybe False (> 1.0) (duration n))
                         (collectAllNodes $ rootNode diags)

  putStrLn "\nSlow initializations (>1s):"
  forM_ slowNodes $ \node -> do
    putStrLn $ "  - " <> T.unpack (nodeName node)
            <> ": " <> show (duration node) <> "s"
```

### Detecting Shared Resources

```haskell
analyzeSharing :: LayerDiagnostics -> IO ()
analyzeSharing diags = do
  putStrLn $ "Total resources: " <> show (totalResources diags)
  putStrLn $ "Shared resources: " <> show (sharedResources diags)
  putStrLn $ "Sharing efficiency: "
          <> show (percentage (sharedResources diags) (totalResources diags))
          <> "%"
```

## Implementation Phases

### Phase 1: Foundation (Current)
- ✅ Data structures defined
- ✅ JSON serialization
- ✅ Terminal rendering
- ✅ Module skeleton

### Phase 2: Basic Integration
- Add diagnostics toggle to LayerEnv
- Instrument `resource` and `effect` constructors
- Basic node tracking

### Phase 3: Advanced Features
- Service caching detection
- Composition tracking (>>>, &&&)
- Error tracking
- Performance metrics

### Phase 4: Enhancements
- Colored terminal output
- Interactive HTML export
- GraphViz DOT format export
- Real-time progress tracking

## Performance Considerations

- Diagnostics collection has minimal overhead (<5%)
- Uses `IORef` for efficient mutable state
- Only enabled when explicitly requested
- Zero cost when disabled

## Testing Strategy

- Unit tests for tree rendering
- Integration tests with example layers
- Performance benchmarks
- Golden tests for JSON output
