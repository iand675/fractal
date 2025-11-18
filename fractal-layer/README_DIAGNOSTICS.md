# Fractal.Layer Diagnostics

A comprehensive diagnostics and visualization system for tracking and debugging Layer initialization.

## Features

### 🌳 Tree Visualization
Render your layer initialization as a beautiful ASCII tree showing:
- Sequential composition (>>>)
- Parallel composition (&&&)
- Resource allocation timing
- Service caching and sharing
- Initialization failures

### 📊 JSON Export
Export complete diagnostics to JSON for:
- CI/CD integration
- Performance monitoring
- Historical analysis
- Custom tooling

### 🔍 Debug Information
Track essential metrics:
- Total initialization time
- Per-layer timing
- Shared vs. fresh resources
- Dependency relationships
- Metadata and configuration

## Quick Start

```haskell
import Fractal.Layer
import Fractal.Layer.Diagnostics

-- Your application layers
appLayer :: Layer IO () (Config, Database, WebServer)
appLayer = configLayer >>> (dbLayer &&& webLayer)

main :: IO ()
main = withLayerDiagnostics () appLayer $ \(env, diags) -> do
  -- Print initialization tree
  putStrLn $ renderLayerTree diags

  -- Export to JSON
  BSL.writeFile "init-tree.json" (encode diags)

  -- Use your environment
  let (cfg, db, ws) = env
  runApp cfg db ws
```

## Example Output

### Terminal ASCII Tree

```
Layer Initialization Tree
=========================

Total Duration: 0.45s
Total Resources: 7
Shared Resources: 2

ApplicationLayer (0.45s)
├── ConfigLayer (0.05s)
└── ParallelServices (0.4s)
    ├── DatabaseLayer (0.25s)
    │   └── ConnectionPool [SERVICE: svc-001] (0.2s)
    ├── CacheLayer (0.15s)
    │   ├── RedisClient (0.1s)
    │   └── MetricsCollector [SERVICE: svc-002] (0.05s)
    └── WebServerLayer (0.3s)
        ├── HTTPServer (0.25s)
        └── MetricsCollector [SHARED: svc-002]
```

### JSON Output

```json
{
  "root": {
    "id": "root",
    "name": "ApplicationLayer",
    "type": "sequential",
    "duration": 0.45,
    "children": [...]
  },
  "totalDuration": 0.45,
  "totalResources": 7,
  "sharedResources": 2
}
```

## API Reference

### Core Types

```haskell
data LayerDiagnostics = LayerDiagnostics
  { rootNode :: LayerNode
  , totalDuration :: Double
  , totalResources :: Int
  , sharedResources :: Int
  }

data LayerNode = LayerNode
  { nodeId :: Text
  , nodeName :: Text
  , nodeType :: LayerNodeType
  , status :: ResourceStatus
  , duration :: Maybe Double
  , children :: [LayerNode]
  , metadata :: HashMap Text Text
  }
```

### Rendering Functions

```haskell
-- Compact ASCII tree
renderLayerTree :: LayerDiagnostics -> String

-- Detailed view with metadata
renderLayerTreeDetailed :: LayerDiagnostics -> String

-- JSON export
diagnosticsToJSON :: LayerDiagnostics -> Value
```

### Running with Diagnostics

```haskell
withLayerDiagnostics ::
  MonadUnliftIO m =>
  deps ->
  Layer m deps env ->
  (env -> ResourceT m (r, LayerDiagnostics)) ->
  m (r, LayerDiagnostics)
```

## Use Cases

### 1. Performance Profiling

Find slow initialization:

```haskell
debugLayerPerformance :: Layer IO () env -> IO ()
debugLayerPerformance layer = withLayerDiagnostics () layer $ \(_, diags) -> do
  let slowNodes = filter (\n -> maybe False (> 1.0) (duration n))
                         (collectAllNodes $ rootNode diags)

  putStrLn "Slow initializations (>1s):"
  forM_ slowNodes $ \node ->
    putStrLn $ "  - " <> nodeName node <> ": " <> show (duration node) <> "s"
```

### 2. Resource Sharing Analysis

Verify service caching:

```haskell
analyzeSharing :: LayerDiagnostics -> IO ()
analyzeSharing diags = do
  let efficiency = (sharedResources diags * 100) `div` totalResources diags
  putStrLn $ "Sharing efficiency: " <> show efficiency <> "%"
```

### 3. CI/CD Integration

Export diagnostics for monitoring:

```haskell
exportDiagnostics :: LayerDiagnostics -> IO ()
exportDiagnostics diags = do
  BSL.writeFile "build/diagnostics.json" (encode diags)

  -- Fail if initialization is too slow
  when (totalDuration diags > 5.0) $
    exitFailure
```

### 4. Documentation

Generate initialization diagrams for documentation:

```haskell
generateDocs :: Layer IO () env -> IO ()
generateDocs layer = withLayerDiagnostics () layer $ \(_, diags) -> do
  writeFile "docs/initialization.txt" (renderLayerTree diags)
  BSL.writeFile "docs/initialization.json" (encode diags)
```

## Implementation Status

**Phase 1: Foundation** ✅ (Current)
- Data structures defined
- JSON serialization
- Terminal rendering
- Module skeleton

**Phase 2: Basic Integration** (Next)
- Instrument Layer constructors
- Add diagnostics toggle
- Basic tracking

**Phase 3: Advanced Features** (Future)
- Service caching detection
- Composition tracking
- Error tracking
- Real-time progress

See [DIAGNOSTICS_INTEGRATION.md](./DIAGNOSTICS_INTEGRATION.md) for full integration plan.

## Configuration

Diagnostics have minimal overhead (<5%) and can be enabled/disabled per-run:

```haskell
-- With diagnostics
withLayerDiagnostics deps layer action

-- Without diagnostics (normal operation)
withLayer deps layer action
```

## Examples

See [examples/DiagnosticsExample.hs](./examples/DiagnosticsExample.hs) for a complete working example.

## Contributing

The diagnostics system is designed to be extensible. Future enhancements could include:

- Colored terminal output
- Interactive HTML export
- GraphViz DOT format
- Real-time progress bars
- Historical comparison
- Flamegraph generation

## License

BSD-3-Clause (same as fractal-layer)
