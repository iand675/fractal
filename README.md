# Fractal

A collection of Haskell libraries for building composable, type-safe applications.

## Packages

- **[fractal-layer](./fractal-layer)** - Composable resource management and dependency injection
- **[fractal-stream](./fractal-stream)** - Event streaming with Redis/Kinesis backends (WIP)

---

# Fractal-Layer: Decomposing Monoliths with Type-Safe Resource Management

When breaking apart a monolithic application, one of the most challenging aspects is managing the web of dependencies between services. Database connections, caches, API clients, and configuration all need to be initialized in the right order, shared where appropriate, and cleaned up gracefully. This is where **fractal-layer** shines.

## The Problem: Monolithic Initialization

In a typical Haskell monolith, initialization often looks like this:

```haskell
main :: IO ()
main = do
  -- Load config (needs to happen first)
  config <- loadConfig

  -- Initialize database (needs config)
  pool <- createPool (dbConnectionString config)

  -- Initialize cache (needs config)
  redis <- connectRedis (redisUrl config)

  -- Initialize metrics (needs config)
  metrics <- initMetrics (metricsConfig config)

  -- Initialize API client (needs config AND metrics)
  apiClient <- createApiClient (apiUrl config) metrics

  -- Initialize web server (needs EVERYTHING)
  let app = createApp pool redis metrics apiClient

  -- Hope nothing fails during shutdown!
  run 8080 app
```

This approach has several problems:

1. **No automatic cleanup** - If something fails halfway through, you leak resources
2. **Order dependencies are implicit** - You must mentally track what depends on what
3. **No parallelization** - Everything initializes sequentially, even independent resources
4. **Difficult to test** - Hard to mock individual layers
5. **Brittle refactoring** - Adding a new dependency requires careful manual threading

## Enter Fractal-Layer

Fractal-layer provides a composable abstraction for resource management that solves these problems elegantly:

```haskell
import Fractal.Layer

-- Define your layers independently
configLayer :: Layer IO () Config
configLayer = effect $ \_ -> loadConfig

dbLayer :: Layer IO Config DatabasePool
dbLayer = resource
  (\config -> createPool (dbConnectionString config))
  closePool

redisLayer :: Layer IO Config RedisConnection
redisLayer = resource
  (\config -> connectRedis (redisUrl config))
  disconnect

metricsLayer :: Layer IO Config Metrics
metricsLayer = service $ effect $ \config ->
  initMetrics (metricsConfig config)

apiClientLayer :: Layer IO (Config, Metrics) ApiClient
apiClientLayer = effect $ \(config, metrics) ->
  createApiClient (apiUrl config) metrics

-- Compose them declaratively
appLayer :: Layer IO () App
appLayer =
  configLayer >>> (dbLayer &&& redisLayer &&& metricsLayer) >>> \(db, redis, metrics, config) ->
    apiClientLayer >>> \client ->
      pure $ App db redis metrics client

main :: IO ()
main = withLayer appLayer () $ \app ->
  run 8080 (handler app)
```

## Key Benefits

### 1. **Automatic Resource Cleanup**

Resources are automatically cleaned up in reverse order, even if initialization fails partway through:

```haskell
dbLayer :: Layer IO Config DatabasePool
dbLayer = resource
  (\config -> createPool (dbConnectionString config))  -- Acquire
  closePool                                             -- Release (automatic!)
```

Under the hood, this uses `ResourceT` to ensure exception-safe cleanup.

### 2. **Explicit Dependency Graph**

Dependencies are expressed in the type signature:

```haskell
-- This layer needs Config and Metrics
apiClientLayer :: Layer IO (Config, Metrics) ApiClient

-- This layer needs nothing
configLayer :: Layer IO () Config
```

The compiler prevents you from composing layers incorrectly!

### 3. **Parallel Initialization**

Use `&&&` to initialize independent resources in parallel:

```haskell
-- DB, Redis, and Metrics all initialize concurrently
configLayer >>> (dbLayer &&& redisLayer &&& metricsLayer)
```

This can dramatically speed up application startup when you have many independent resources.

### 4. **Service Caching**

Mark expensive resources as `service` to share them across the dependency graph:

```haskell
metricsLayer :: Layer IO Config Metrics
metricsLayer = service $ effect $ \config ->
  initMetrics (metricsConfig config)
```

If multiple layers depend on `Metrics`, it's initialized only once and shared. Perfect for connection pools, API clients, or other expensive singletons.

### 5. **Visibility into Initialization**

The diagnostics system shows you exactly what's happening:

```haskell
main :: IO ()
main = withLayerDiagnostics appLayer () $ \(app, diags) -> do
  putStrLn $ renderLayerTree diags
  run 8080 (handler app)
```

Output:
```
Layer Initialization Tree
═════════════════════════

⧗ Duration: 2.3s
◆ Resources: 5
↻ Shared: 1

└── ⊕ Root ⧗2.3s ✓
    ├── ⚡ Config ⧗0.05s ✓
    └── ⋈ ParallelComposition ⧗2.1s ✓
        ├── ◆ DatabasePool ⧗1.2s ✓
        ├── ◆ RedisConnection ⧗0.8s ✓
        └── ◉ Metrics ⧗0.1s ✓
```

You can even watch initialization in real-time with live terminal rendering!

## Installation

```bash
cabal install fractal-layer
```

Or add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - fractal-layer
```

## Real-World Migration Strategy

When decomposing a monolith, you can adopt fractal-layer incrementally:

### Phase 1: Wrap Existing Initialization

```haskell
-- Wrap your existing initialization code
legacyInitLayer :: Layer IO () LegacyApp
legacyInitLayer = effect $ \_ -> do
  config <- loadConfig
  pool <- createPool config
  redis <- connectRedis config
  pure $ LegacyApp config pool redis
```

### Phase 2: Extract Individual Layers

```haskell
-- Start breaking out pieces
configLayer :: Layer IO () Config
configLayer = effect $ \_ -> loadConfig

dbLayer :: Layer IO Config DatabasePool
dbLayer = resource
  (\config -> createPool (dbConnectionString config))
  closePool

-- Keep the rest monolithic for now
restOfAppLayer :: Layer IO (Config, DatabasePool) LegacyApp
restOfAppLayer = effect $ \(config, pool) -> do
  redis <- connectRedis config
  pure $ LegacyApp config pool redis

-- Compose what you have
appLayer :: Layer IO () LegacyApp
appLayer = configLayer >>> dbLayer >>> restOfAppLayer
```

### Phase 3: Full Decomposition

Eventually, extract all layers for maximum composability:

```haskell
appLayer :: Layer IO () App
appLayer =
  configLayer >>>
  (dbLayer &&& redisLayer &&& metricsLayer) >>>
  apiClientLayer >>>
  webServerLayer
```

## Testing Benefits

Fractal-layer makes testing dramatically easier:

```haskell
-- Production: real database
prodDbLayer :: Layer IO Config DatabasePool
prodDbLayer = resource
  (\config -> createPool (dbConnectionString config))
  closePool

-- Testing: in-memory mock
testDbLayer :: Layer IO Config DatabasePool
testDbLayer = effect $ \_ -> pure inMemoryPool

-- Same composition, different implementation
testAppLayer :: Layer IO () App
testAppLayer =
  testConfigLayer >>>
  (testDbLayer &&& testRedisLayer) >>>
  appLogicLayer
```

## Advanced: Custom Instrumentation

The interceptor pattern allows you to plug in custom behavior:

```haskell
-- Log all resource acquisitions
loggingInterceptor :: LayerInterceptor IO
loggingInterceptor = nullInterceptor
  { onResourceAcquire = \ctx ->
      putStrLn $ "Acquiring: " <> operationName ctx
  , onResourceRelease = \name duration ->
      putStrLn $ "Released: " <> name <> " in " <> show duration
  }

main :: IO ()
main = withLayerInterceptor loggingInterceptor appLayer () $ \app ->
  run 8080 (handler app)
```

This is perfect for:
- Performance monitoring
- Distributed tracing integration
- Custom metrics collection
- Debugging initialization issues

## API Overview

### Core Types

```haskell
-- A Layer transforms dependencies into a resource
data Layer m deps env

-- Build a layer into a resource
build :: Layer m deps env -> deps -> m env

-- Run a layer with automatic cleanup
withLayer :: Layer IO deps env -> deps -> (env -> IO a) -> IO a
```

### Layer Constructors

```haskell
-- Pure effect (no cleanup needed)
effect :: (deps -> m env) -> Layer m deps env

-- Managed resource (automatic cleanup)
resource :: (deps -> m env) -> (env -> m ()) -> Layer m deps env

-- Cached singleton service
service :: Layer m deps env -> Layer m deps env
```

### Layer Combinators

```haskell
-- Sequential composition
(>>>) :: Layer m deps mid -> Layer m mid env -> Layer m deps env

-- Parallel composition
(&&&) :: Layer m deps a -> Layer m deps b -> Layer m deps (a, b)

-- Functor mapping
(<$>) :: (a -> b) -> Layer m deps a -> Layer m deps b
```

### Diagnostics

```haskell
-- Run with diagnostics collection
withLayerDiagnostics :: Layer IO deps env -> deps -> ((env, LayerDiagnostics) -> IO a) -> IO a

-- Render diagnostics tree
renderLayerTree :: LayerDiagnostics -> String
renderLayerTreeDetailed :: LayerDiagnostics -> String

-- Live terminal rendering
renderLayerTreeLive :: DiagnosticsCollector -> IO Bool -> IO ()
```

## Comparison with Other Approaches

### vs. ReaderT Pattern

**ReaderT:**
```haskell
main = do
  config <- loadConfig
  pool <- createPool config
  runReaderT app (config, pool)
  -- Manual cleanup, no parallelization
```

**Fractal-Layer:**
```haskell
main = withLayer (configLayer >>> dbLayer) () $ \(config, pool) ->
  runApp config pool
  -- Automatic cleanup, parallel initialization
```

### vs. Registry Pattern

**Registry:** Global mutable registry with runtime dependency resolution

**Fractal-Layer:** Compile-time dependency checking with type-safe composition

### vs. Manual ResourceT

**Manual ResourceT:** Explicit resource management, verbose

**Fractal-Layer:** Declarative composition, automatic resource tracking

## Documentation

- [API Documentation](./fractal-layer/src/Fractal/Layer.hs)
- [Diagnostics Guide](./fractal-layer/src/Fractal/Layer/Diagnostics.hs)
- [Interceptor Pattern](./fractal-layer/src/Fractal/Layer/Interceptor.hs)

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](./CONTRIBUTING.md) for guidelines.

## License

BSD-3-Clause

## Summary

Breaking apart a monolith requires careful management of resources, dependencies, and initialization order. Fractal-layer provides a type-safe, composable approach that:

- ✓ Guarantees resource cleanup
- ✓ Makes dependencies explicit
- ✓ Enables parallelization
- ✓ Provides visibility into initialization
- ✓ Simplifies testing
- ✓ Supports incremental adoption

Instead of fighting with imperative initialization code, you can build your application from composable, reusable layers that express *what* you need, not *how* to wire it together.

The best part? You don't have to rewrite your entire monolith at once. Start small, wrap what you have, and gradually extract layers as you decompose your application.
