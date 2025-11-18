{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Fractal.Layer
-- Description : A composable resource management and dependency injection system
-- Stability   : experimental
-- Portability : Portable
--
-- The @Layer@ type provides a powerful abstraction for managing resources and dependencies
-- in a composable way. It's particularly useful for building complex systems where
-- multiple services need to be initialized, composed, and properly cleaned up.
--
-- == Key Features
--
-- * Resource management with automatic cleanup
-- * Type-safe dependency injection
-- * Composable service layers
-- * Concurrent resource initialization
-- * Exception-safe resource handling
--
-- == Basic Usage
--
-- Here's a simple example of creating and composing layers:
--
-- @
-- data Config = Config { port :: Int, host :: String }
-- data Database = Database { connection :: Connection }
-- data WebServer = WebServer { server :: Server }
--
-- -- Create individual layers
-- configLayer :: Layer IO () Config
-- configLayer = effect $ \_ -> pure $ Config 8080 "localhost"
--
-- dbLayer :: Layer IO Config Database
-- dbLayer = resource
--   (\cfg -> connectDB (host cfg) (port cfg))
--   (\db -> closeConnection (connection db))
--
-- webLayer :: Layer IO (Config, Database) WebServer
-- webLayer = resource
--   (\(cfg, db) -> startServer (port cfg) (connection db))
--   (\ws -> stopServer (server ws))
--
-- -- Compose layers
-- appLayer :: Layer IO () (Config, Database, WebServer)
-- appLayer = configLayer >>> (dbLayer &&& webLayer)
--
-- -- Run the application
-- main :: IO ()
-- main = withLayer appLayer () $ \(cfg, db, ws) -> do
--   -- Use the services
--   serveRequests (server ws)
-- @
--
-- == Advanced Composition
--
-- Layers can be composed both vertically (sequentially) and horizontally (in parallel):
--
-- @
-- -- Vertical composition (sequentially)
-- combinedLayer :: Layer IO () (Config, Database)
-- combinedLayer = configLayer >>> dbLayer
--
-- -- Horizontal composition (in parallel)
-- parallelLayer :: Layer IO Config (Database, WebServer)
-- parallelLayer = dbLayer &&& webLayer
--
-- -- Complex composition
-- fullSystem :: Layer IO () (Config, Database, WebServer, Cache)
-- fullSystem = configLayer >>> (dbLayer &&& webLayer &&& cacheLayer)
-- @
--
-- == Resource Management
--
-- The @Layer@ type ensures proper resource cleanup through the @ResourceT@ monad transformer.
-- Resources are released in the correct order (LIFO) when the layer goes out of scope:
--
-- @
-- managedResource :: Layer IO () Handle
-- managedResource = resource
--   (\_ -> openFile "data.txt" ReadMode)
--   (\h -> hClose h)
--
-- -- Resources are automatically cleaned up
-- withLayer managedResource () $ \handle -> do
--   content <- hGetContents handle
--   -- Use the handle...
--   -- It will be automatically closed when this block exits
-- @
--
-- == Error Handling
--
-- Layers handle errors gracefully, ensuring proper cleanup even when exceptions occur:
--
-- @
-- fallbackLayer :: Layer IO () Service
-- fallbackLayer = primaryLayer <|> backupLayer
--
-- -- If primaryLayer fails, backupLayer will be tried
-- -- Both layers' resources will be properly cleaned up
-- @
--
-- == Type Safety
--
-- The type system ensures that dependencies are properly satisfied:
--
-- @
-- -- This won't compile if the dependencies don't match
-- type SafeSystem = Layer IO Config (Database, WebServer)
-- safeSystem :: SafeSystem
-- safeSystem = dbLayer &&& webLayer  -- Both layers require Config
-- @
module Fractal.Layer
  ( Layer (..),
    LayerEnv (..),
    build,
    runLayer,
    runLayerWithInterceptor,
    withLayer,
    withLayerAndInterceptor,
    mkLayer,
    mapLayer,
    zipLayer,
    composeLayer,
    resource,
    effect,
    bracketed,

    -- * Services
    Service,
    mkService,
    service,
    uncached,

    -- * Building reusable environments
    Environment,
    emptyEnvironment,
    lookupEnvironment,
    mergeEnvironment,
    appendEnvironment,
    AssembleFromEnvironment (..),
    Assembled,

    -- * Re-exports
    MonadResource,
  )
where

import Control.Applicative
import Control.Arrow
import Control.Category
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (createInternalState, runInternalState, withInternalState)
import Control.Monad.Trans.Resource.Internal (ReleaseMap (..), stateCleanupChecked)
import Control.Selective
import Data.Either
import Data.Function hiding (id, (.))
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import Data.IntMap.Strict (mapKeysMonotonic)
import Data.Profunctor
import Data.Profunctor.Traversing
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Traversable
import Data.Tuple
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Fractal.Layer.Interceptor
import UnliftIO
import UnliftIO.Resource
import Unsafe.Coerce
import Prelude hiding ((.))
import GHC.Exts (Any)

data ServiceState a
  = Initialized a
  | Failed SomeException

data LayerEnv m = LayerEnv
  { serviceStates :: !(MVar (HashMap TypeRep (ServiceState Any)))
  , interceptor :: !(LayerInterceptor m)
  }

newtype Service m deps env = Service
  { uncached :: Layer m deps env
  }

getOrCreateCachedService :: forall m deps env. (MonadUnliftIO m, Typeable env) => Service m deps env -> Layer m deps env
getOrCreateCachedService (Service m) = Layer $ \lenv env -> do
  let rep = typeRep (Proxy @env)
      serviceName = T.pack (show rep)

  -- Try to get the current state
  states <- liftIO $ readMVar lenv.serviceStates
  case HashMap.lookup rep states of
    Just (Initialized x) -> do
      -- Service already exists, notify reuse
      lift $ onServiceReuse (interceptor lenv) serviceName rep
      pure $ unsafeCoerce x
    Just (Failed e) -> throwIO e
    _ -> join $ modifyMVar lenv.serviceStates $ \serviceStates -> do
      case HashMap.lookup rep serviceStates of
        Just (Initialized x) -> do
          -- Another thread initialized it
          lift $ onServiceReuse (interceptor lenv) serviceName rep
          pure (serviceStates, pure $ unsafeCoerce x)
        Just (Failed e) -> pure (serviceStates, throwIO e)
        Nothing -> do
          -- Notify service creation
          let ctx = OperationContext
                { operationName = serviceName
                , operationType = Just rep
                , operationMetadata = []
                }
          lift $ onServiceCreate (interceptor lenv) ctx

          -- Initialize the service
          eRes <- try $ build m lenv env
          case eRes of
            Left e -> do
              let states' = HashMap.insert rep (Failed e) states
              pure (states', throwIO e)
            Right x -> do
              let states' = HashMap.insert rep (Initialized $ unsafeCoerce x) states
              pure (states', pure x)

-- |
-- A @Layer m deps env@ describes how to build an environment @env@
-- given a set of dependencies @deps@. Resource acquisition and
-- cleanup are handled transparently via @ResourceT@, so that finalizers
-- are executed in the correct (LIFO) order once the environment goes
-- out of scope.
--
-- The type parameters are:
-- * @m@ - The base monad (typically @IO@)
-- * @deps@ - The dependencies required to build this layer
-- * @env@ - The environment produced by this layer
--
-- Example:
-- @
-- type DatabaseLayer = Layer IO Config Database
-- type WebLayer = Layer IO (Config, Database) WebServer
--
-- -- A layer that requires no dependencies
-- configLayer :: Layer IO () Config
-- configLayer = effect $ \_ -> pure $ Config 8080 "localhost"
--
-- -- A layer that depends on Config
-- dbLayer :: DatabaseLayer
-- dbLayer = resource
--   (\cfg -> connectDB (host cfg) (port cfg))
--   (\db -> closeConnection (connection db))
-- @
newtype Layer m deps env = Layer
  { build :: LayerEnv m -> deps -> ResourceT m env
  -- ^ Build the layer inside 'ResourceT', acquiring any required
  --   resources and registering their corresponding finalizers.
  }

-------------------------------------------------------------------------------
-- Construction helpers
-------------------------------------------------------------------------------

-- | Construct a pure layer with no side-effects or resource demands.
pureLayer :: Applicative m => env -> Layer m deps env
pureLayer env = Layer $ \_ -> const (pure env)

-- | Lowest-level smart constructor for effectful layers.
mkLayer :: (deps -> ResourceT m env) -> Layer m deps env
mkLayer f = Layer $ \_ -> f

-- |
-- Lift an acquire/release pair into a managed layer.
--
-- The returned layer registers the @release@ action as a finalizer, so it
-- is guaranteed to run – even in the presence of exceptions.
--
-- Example:
-- @
-- fileLayer :: Layer IO () Handle
-- fileLayer = resource
--   (\_ -> openFile "data.txt" ReadMode)  -- acquire
--   (\h -> hClose h)                      -- release
--
-- -- The file will be automatically closed when the layer is released
-- withLayer fileLayer () $ \handle -> do
--   content <- hGetContents handle
--   -- Use the handle...
-- @
resource ::
  forall m deps env. (MonadUnliftIO m, Typeable env) =>
  -- | Acquire resource
  (deps -> m env) ->
  -- | Release resource
  (env -> m ()) ->
  Layer m deps env
resource acq rel = Layer $ \lenv deps -> do
  -- Notify interceptor
  let ctx = OperationContext
        { operationName = T.pack (show $ typeRep (Proxy @env))
        , operationType = Just (typeRep (Proxy @env))
        , operationMetadata = []
        }
  lift $ onResourceAcquire (interceptor lenv) ctx
  startTime <- liftIO getCurrentTime

  -- Original logic
  (_, env) <- allocateU (lift $ acq deps) (lift . rel)

  -- Notify interceptor on release
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  lift $ onResourceRelease (interceptor lenv) (operationName ctx) duration

  pure env
{-# SPECIALIZE resource :: forall deps env. Typeable env => (deps -> IO env) -> (env -> IO ()) -> Layer IO deps env #-}

-- |
-- Lift a monadic function into a layer without requiring cleanup.
--
-- Unlike 'resource', this function doesn't register a finalizer, making it
-- suitable for operations that don't acquire resources needing cleanup.
--
-- Example:
-- @
-- configLayer :: Layer IO () Config
-- configLayer = effect $ \_ -> do
--   port <- readEnvVar "PORT"
--   host <- readEnvVar "HOST"
--   pure $ Config (read port) host
-- @
effect ::
  forall m deps env. (MonadUnliftIO m, Typeable env) =>
  -- | Run effect to produce environment
  (deps -> m env) ->
  Layer m deps env
effect f = Layer $ \lenv deps -> do
  -- Notify interceptor
  let ctx = OperationContext
        { operationName = T.pack (show $ typeRep (Proxy @env))
        , operationType = Just (typeRep (Proxy @env))
        , operationMetadata = []
        }
  lift $ onEffectRun (interceptor lenv) ctx
  startTime <- liftIO getCurrentTime

  -- Run effect
  result <- lift $ f deps

  -- Notify completion
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  lift $ onEffectComplete (interceptor lenv) (operationName ctx) duration

  pure result
{-# SPECIALIZE effect :: forall deps env. Typeable env => (deps -> IO env) -> Layer IO deps env #-}

-- |
-- When you want to run a scoped effect that acquires a resource and releases it, you maybe want it to survive
-- the lifetime of the outputted environment. This function helps you do that.
--
-- If you can initialize the resource yourself, it's better to use 'resource', but you may be working with a library.
-- that doesn't provide a way to do that.
--
-- Example:
-- @
-- pool <- bracketed withResourcePool
-- @
bracketed ::
  MonadUnliftIO m =>
  (forall a. deps -> (env -> m a) -> m a) ->
  Layer m deps env
bracketed f = Layer $ \_ deps -> do
  resourceVar <- newEmptyMVar
  cleanupVar <- newEmptyMVar
  void $ allocateU
    ( async do
      lift $ f deps (\env -> putMVar resourceVar env >> takeMVar cleanupVar)
    )
    (\h -> do
      putMVar cleanupVar ()
      wait h
    )
  takeMVar resourceVar

-- | Turn a layer into a service.
-- |
-- Turn a layer into a service that can be cached and shared across multiple dependents.
-- Services are useful for expensive-to-initialize resources that should be shared
-- rather than duplicated.
--
-- Example:
-- @
-- -- Define a connection pool that should be shared
-- data ConnectionPool = ConnectionPool
--   { connections :: [Connection]
--   , maxSize :: Int
--   }
--
-- -- Create the pool layer
-- poolLayer :: Layer IO Config ConnectionPool
-- poolLayer = resource
--   (\cfg -> do
--     putStrLn "Initializing expensive connection pool..."
--     conns <- replicateM 10 $ connect (dbHost cfg)
--     pure $ ConnectionPool conns 10)
--   (\pool -> do
--     putStrLn "Closing all connections..."
--     mapM_ close (connections pool))
--
-- -- Convert to a service
-- poolService :: Service IO Config ConnectionPool
-- poolService = mkService poolLayer
-- @
mkService :: Layer m deps env -> Service m deps env
mkService = Service

-- |
-- A service is a layer that is cached and shared between all layers that depend on it.
--
-- This is useful for services that are expensive to initialize, but are shared between many layers.
-- |
-- Convert a 'Service' into a 'Layer' that caches and shares the initialized resource.
--
-- Services provide singleton-like behavior within a layer composition. The first time
-- a service is requested, it will be initialized and cached. All subsequent requests
-- will receive the same instance, regardless of how many times or from where it's requested.
--
-- == Key Properties
--
-- * __Thread-safe initialization__: Multiple concurrent requests for the same service
--   will result in only one initialization, with others waiting for it to complete.
-- * __Type-based caching__: Services are cached based on their result type, so you
--   can only have one service per type in a composition.
-- * __Shared lifecycle__: The service's resources are managed by the overall layer
--   composition and will be cleaned up when the composition is released.
-- * __Error propagation__: If service initialization fails, the error is propagated
--   to all waiting consumers.
--
-- Example:
-- @
-- -- Expensive service that should only be initialized once
-- metricsService :: Service IO Config MetricsCollector
-- metricsService = mkService $ resource
--   (\cfg -> do
--     putStrLn "Starting metrics collector (expensive)..."
--     collector <- initMetrics (metricsEndpoint cfg)
--     startCollector collector
--     pure collector)
--   (\collector -> do
--     putStrLn "Stopping metrics collector..."
--     stopCollector collector)
--
-- -- Multiple layers can use the same service
-- webLayer :: Layer IO Config WebServer
-- webLayer = do
--   metrics <- service metricsService  -- First call initializes
--   resource
--     (\cfg -> startWebServer (port cfg) metrics)
--     stopWebServer
--
-- apiLayer :: Layer IO Config ApiServer
-- apiLayer = do
--   metrics <- service metricsService  -- Reuses existing instance
--   resource
--     (\cfg -> startApiServer (apiPort cfg) metrics)
--     stopApiServer
--
-- -- Both servers share the same metrics collector
-- appLayer :: Layer IO Config (WebServer, ApiServer)
-- appLayer = liftA2 (,) webLayer apiLayer
-- @
--
-- == Important Notes
--
-- * Services must have a 'Typeable' constraint on their output type for caching
-- * Only one service instance per type can exist in a layer composition
-- * Services are not cleaned up until the entire layer composition is released
-- * Initialization happens on first access, not at service definition time
service :: forall m deps env. (MonadUnliftIO m, Typeable env) => Service m deps env -> Layer m deps env
service = getOrCreateCachedService

-------------------------------------------------------------------------------
-- Typeclass instances --------------------------------------------------------
-------------------------------------------------------------------------------

instance Functor m => Functor (Layer m deps) where
  {-# SPECIALIZE instance Functor (Layer IO deps) #-}
  fmap f (Layer l) = Layer $ \lenv deps -> fmap f (l lenv deps)

instance MonadUnliftIO m => Applicative (Layer m deps) where
  {-# SPECIALIZE instance Applicative (Layer IO deps) #-}
  pure = pureLayer
  lf <*> la = dimap (\d -> (d, d)) (uncurry ($)) (zipLayer lf la)

instance MonadUnliftIO m => Selective (Layer m deps) where
  {-# SPECIALIZE instance Selective (Layer IO deps) #-}
  select = selectM

instance MonadUnliftIO m => Monad (Layer m deps) where
  {-# SPECIALIZE instance Monad (Layer IO deps) #-}
  return = pure
  (Layer l) >>= f = Layer $ \lenv deps -> do
    a <- l lenv deps
    let Layer l' = f a
    l' lenv deps

instance (MonadUnliftIO m, Semigroup env) => Semigroup (Layer m deps env) where
  {-# SPECIALIZE instance Semigroup env => Semigroup (Layer IO deps env) #-}
  lf <> la = liftA2 (<>) lf la

instance (MonadUnliftIO m, Monoid env) => Monoid (Layer m deps env) where
  {-# SPECIALIZE instance Monoid env => Monoid (Layer IO deps env) #-}
  mempty = pure mempty

mapLayer :: (b -> a) -> Layer m a c -> Layer m b c
mapLayer f (Layer l) = Layer $ \lenv env -> l lenv (f env)

instance (MonadUnliftIO m) => MonadReader deps (Layer m deps) where
  ask = Layer $ \_ deps -> pure deps
  local f (Layer l) = Layer $ \lenv deps -> l lenv (f deps)

-- | Vertical composition: feed the output of the first layer as the
--   dependencies of the second.
composeLayer ::
  Monad m =>
  -- | "Upstream" layer
  Layer m a b ->
  -- | "Downstream" layer
  Layer m b c ->
  Layer m a c
composeLayer (Layer upper) (Layer lower) = Layer $ \lenv -> upper lenv >=> lower lenv
{-# SPECIALIZE composeLayer :: Layer IO a b -> Layer IO b c -> Layer IO a c #-}

instance Monad m => Category (Layer m) where
  {-# SPECIALIZE instance Category (Layer IO) #-}
  id = Layer $ \_ deps -> pure deps
  (.) = flip composeLayer

instance Functor m => Profunctor (Layer m) where
  {-# SPECIALIZE instance Profunctor (Layer IO) #-}
  dimap f g (Layer l) = Layer $ \lenv a -> fmap g (l lenv (f a))
  lmap  = mapLayer
  rmap = fmap

instance Monad m => Strong (Layer m) where
  {-# SPECIALIZE instance Strong (Layer IO) #-}
  first' :: Layer m a b -> Layer m (a, c) (b, c)
  first' (Layer l) = Layer $ \lenv (a, c) -> do
    b <- l lenv a
    pure (b, c)

  second' :: Layer m a b -> Layer m (c, a) (c, b)
  second' (Layer l) = Layer $ \lenv (c, a) -> do
    b <- l lenv a
    pure (c, b)

instance Monad m => Choice (Layer m) where
  {-# SPECIALIZE instance Choice (Layer IO) #-}
  left' :: Layer m a b -> Layer m (Either a c) (Either b c)
  left' (Layer l) = Layer $ \lenv -> \case
    Left a -> Left <$> l lenv a
    Right c -> pure (Right c)

  right' :: Layer m a b -> Layer m (Either c a) (Either c b)
  right' (Layer l) = Layer $ \lenv -> \case
    Right a -> Right <$> l lenv a
    Left c -> pure (Left c)

instance Monad m => Traversing (Layer m) where
  {-# SPECIALIZE instance Traversing (Layer IO) #-}
  traverse' :: Traversable f => Layer m a b -> Layer m (f a) (f b)
  traverse' (Layer l) = Layer $ \lenv fa -> traverse (l lenv) fa

instance Monad m => Arrow (Layer m) where
  {-# SPECIALIZE instance Arrow (Layer IO) #-}
  arr f = Layer $ \_ a -> pure (f a)
  first = first'
  second = second'

-------------------------------------------------------------------------------

-- | Exception used for 'empty' in the 'Alternative' instance.

-------------------------------------------------------------------------------

-- |
-- Exception used for 'empty' in the 'Alternative' instance.
-- This is thrown when a layer fails to produce a value, allowing
-- for fallback behavior through the 'Alternative' instance.
data EmptyLayer = EmptyLayer
  deriving stock (Show)

instance Exception EmptyLayer

-- |
-- The 'Alternative' instance allows for fallback behavior between layers.
-- If the first layer fails (throws 'EmptyLayer'), the second layer will be tried.
--
-- Example:
-- @
-- fallbackSystem :: Layer IO Config Service
-- fallbackSystem = primaryService <|> backupService
--
-- -- If primaryService fails, backupService will be tried
-- withLayer fallbackSystem config $ \service -> do
--   -- Use the service...
-- @
instance MonadUnliftIO m => Alternative (Layer m deps) where
  {-# SPECIALIZE instance Alternative (Layer IO deps) #-}
  empty = Layer $ \_ _ -> throwIO EmptyLayer
  Layer l1 <|> Layer l2 = Layer $ \lenv deps ->
    tryWithCleanup (l1 lenv deps) (l2 lenv deps)

-- |
-- The 'MonadPlus' instance provides the same fallback behavior as 'Alternative',
-- with 'mzero' being equivalent to 'empty' and 'mplus' being equivalent to '<|>'.
instance MonadUnliftIO m => MonadPlus (Layer m deps) where
  {-# SPECIALIZE instance MonadPlus (Layer IO deps) #-}
  mzero = empty
  mplus = (<|>)

-- |
-- The 'ArrowZero' instance provides a way to create a layer that always fails.
-- This is useful for implementing conditional behavior in arrow notation.
--
-- Example:
-- @
-- conditionalLayer :: Layer IO Config Service
-- conditionalLayer = proc cfg -> do
--   if shouldUseService cfg
--     then serviceLayer -< cfg
--     else zeroArrow -< cfg
-- @
instance MonadUnliftIO m => ArrowZero (Layer m) where
  {-# SPECIALIZE instance ArrowZero (Layer IO) #-}
  zeroArrow = Layer $ \_ _ -> throwIO EmptyLayer

-- |
-- The 'ArrowPlus' instance allows for fallback behavior in arrow notation.
-- If the first arrow fails, the second will be tried.
--
-- Example:
-- @
-- fallbackArrow :: Layer IO Config Service
-- fallbackArrow = proc cfg -> do
--   primaryService -< cfg
--   <+> backupService -< cfg
-- @
instance MonadUnliftIO m => ArrowPlus (Layer m) where
  {-# SPECIALIZE instance ArrowPlus (Layer IO) #-}
  (Layer l1) <+> (Layer l2) = Layer $ \lenv deps ->
    tryWithCleanup (l1 lenv deps) (l2 lenv deps)

-- |
-- The 'ArrowApply' instance allows for dynamic composition of layers.
-- This is particularly useful when you need to choose which layer to run
-- based on runtime information.
--
-- Example:
-- @
-- dynamicLayer :: Layer IO (Layer IO Config Service, Config) Service
-- dynamicLayer = proc (layer, cfg) -> do
--   app -< (layer, cfg)  -- Apply the layer to the config
-- @
instance Monad m => ArrowApply (Layer m) where
  {-# SPECIALIZE instance ArrowApply (Layer IO) #-}
  app = Layer $ \lenv (Layer l, b) -> l lenv b

-- |
-- Helper function for implementing fallback behavior with proper resource cleanup.
-- This function ensures that resources are properly cleaned up when switching
-- between alternative layers.
--
-- The function:
-- 1. Creates a temporary state for the first layer
-- 2. Attempts to run the first layer
-- 3. If it fails, cleans up and tries the second layer
-- 4. If it succeeds, merges the resources into the main state
--
-- This is used internally by the 'Alternative' and 'ArrowPlus' instances.
tryWithCleanup :: MonadUnliftIO m => ResourceT m a -> ResourceT m a -> ResourceT m a
tryWithCleanup l r = mask $ \restore -> do
  tmpState <- liftIO createInternalState
  try (restore $ runInternalState (lift l) tmpState) >>= \case
    Left e -> do
      liftIO $ stateCleanupChecked (Just (e :: SomeException)) tmpState
      restore r
    Right result -> withInternalState $ \stateMain -> do
      tmpState' <- liftIO $ readIORef tmpState
      atomicModifyIORef' stateMain $ \state -> (unsafelyMergeReleaseMap state tmpState', ())
      pure result

-------------------------------------------------------------------------------
-- Composition
-------------------------------------------------------------------------------

-- |
-- Parallel horizontal composition. Both sub-layers are constructed in parallel
-- and share the same resource scope, ensuring their finalizers are executed
-- together when the parent environment is released.
--
-- Each of the computations is run in isolation, and their resources are merged
-- into the parent scope if they succeed. Otherwise, their resources allocated
-- specific to their scope are released.
--
-- Example:
-- @
-- -- Compose database and web server layers in parallel
-- fullSystem :: Layer IO Config (Database, WebServer)
-- fullSystem = zipLayer dbLayer webLayer
--
-- -- Both services will be initialized concurrently
-- -- If either fails, both will be properly cleaned up
-- withLayer fullSystem config $ \(db, ws) -> do
--   -- Use both services...
-- @
zipLayer ::
  (MonadUnliftIO m) =>
  Layer m d1 o1 ->
  Layer m d2 o2 ->
  Layer m (d1, d2) (o1, o2)
zipLayer (Layer l1) (Layer l2) = Layer $ \lenv (d1, d2) -> do
  -- For concurrent execution, we need to:
  -- 1. Run each layer in isolation to prevent cross-contamination if one fails
  -- 2. Capture their resources and transfer to parent context if successful

  -- Create isolated states for each branch
  stateA <- liftIO createInternalState
  stateB <- liftIO createInternalState

  mask $ \restore -> do
    asyncA <- async $ restore $ runInternalState (lift $ l1 lenv d1) stateA
    asyncB <- async $ restore $ runInternalState (lift $ l2 lenv d2) stateB
    eRes <- try (restore (atomically $ (,) <$> waitCatchSTM asyncA <*> waitCatchSTM asyncB))
    stateA' <- liftIO $ readIORef stateA
    stateB' <- liftIO $ readIORef stateB
    case eRes of
      Left e -> do
        uninterruptibleCancel asyncA
        uninterruptibleCancel asyncB
        liftIO (stateCleanupChecked (Just e) stateA `finally` stateCleanupChecked (Just e) stateB)
        throwIO e
      Right res -> case res of
        (Left eA, Left eB) -> do
          liftIO (stateCleanupChecked (Just eA) stateA `finally` stateCleanupChecked (Just eB) stateB)
          throwIO eA
        (Left eA, Right _) -> do
          liftIO (stateCleanupChecked (Just eA) stateA `finally` stateCleanupChecked Nothing stateB)
          throwIO eA
        (Right _envA, Left eB) -> do
          liftIO (stateCleanupChecked (Just eB) stateB `finally` stateCleanupChecked Nothing stateA)
          throwIO eB
        (Right envA, Right envB) -> withInternalState $ \stateMain -> do
          let mergedStates = unsafelyMergeReleaseMap stateA' stateB'
          atomicModifyIORef' stateMain $ \state -> (unsafelyMergeReleaseMap state mergedStates, ())
          pure (envA, envB)
{-# SPECIALIZE zipLayer :: Layer IO d1 o1 -> Layer IO d2 o2 -> Layer IO (d1, d2) (o1, o2) #-}

-- The first map is the one that is being merged into. Its refcount is not affected.
unsafelyMergeReleaseMap :: ReleaseMap -> ReleaseMap -> ReleaseMap
unsafelyMergeReleaseMap (ReleaseMap nextKey refCount stateA) (ReleaseMap nextKey' _ stateB) = ReleaseMap (nextKey + nextKey') refCount (stateA <> mapKeysMonotonic (+ nextKey) stateB)
unsafelyMergeReleaseMap l ReleaseMapClosed = l
unsafelyMergeReleaseMap ReleaseMapClosed r = r

-------------------------------------------------------------------------------
-- Running layers
-------------------------------------------------------------------------------

-- | Build a layer and obtain its environment.  The acquired resources are
--   released /immediately after/ the environment is produced, so any handles
--   contained within will be invalid. In most cases you want
--   'withLayer' instead- this function exists mainly for quick tests.
runLayer :: MonadUnliftIO m => deps -> Layer m deps env -> m env
runLayer deps (Layer l) = do
  lenv <- LayerEnv <$> newMVar HashMap.empty <*> pure nullInterceptor
  runResourceT (l lenv deps)
{-# SPECIALIZE runLayer :: deps -> Layer IO deps env -> IO env #-}

-- |
-- Safely build a layer, use its environment, and guarantee cleanup.
--
-- @withLayer layer deps useEnv@ acquires the environment, passes it to
-- @useEnv@, and releases all resources when @useEnv@ completes (whether
-- normally or by throwing an exception).
--
-- Example:
-- @
-- main :: IO ()
-- main = withLayer appLayer config $ \(db, ws) -> do
--   -- Use the services...
--   -- Resources will be automatically cleaned up when this block exits
--   serveRequests (server ws)
-- @
withLayer ::
  MonadUnliftIO m =>
  -- | Dependencies
  deps ->
  -- | Layer to run
  Layer m deps env ->
  -- | Action that needs the env
  (env -> ResourceT m r) ->
  m r
withLayer deps (Layer l) useEnv = runResourceT $ do
  lenv <- LayerEnv <$> newMVar HashMap.empty <*> pure nullInterceptor
  env <- l lenv deps
  useEnv env

-- |
-- Run a layer with a custom interceptor.
-- This allows you to instrument layer operations for diagnostics, logging, etc.
--
-- Example:
-- @
-- myInterceptor <- createDiagnosticsInterceptor collector
-- result <- runLayerWithInterceptor myInterceptor () myLayer
-- @
runLayerWithInterceptor ::
  MonadUnliftIO m =>
  -- | Custom interceptor
  LayerInterceptor m ->
  -- | Dependencies
  deps ->
  -- | Layer to run
  Layer m deps env ->
  m env
runLayerWithInterceptor interceptor deps (Layer l) = do
  lenv <- LayerEnv <$> newMVar HashMap.empty <*> pure interceptor
  runResourceT (l lenv deps)
{-# SPECIALIZE runLayerWithInterceptor :: LayerInterceptor IO -> deps -> Layer IO deps env -> IO env #-}

-- |
-- Run a layer with a custom interceptor and use the environment.
-- Similar to 'withLayer' but allows custom instrumentation via an interceptor.
--
-- Example:
-- @
-- myInterceptor <- createDiagnosticsInterceptor collector
-- withLayerAndInterceptor myInterceptor config myLayer $ \env -> do
--   -- Use the environment...
-- @
withLayerAndInterceptor ::
  MonadUnliftIO m =>
  -- | Custom interceptor
  LayerInterceptor m ->
  -- | Dependencies
  deps ->
  -- | Layer to run
  Layer m deps env ->
  -- | Action that needs the env
  (env -> ResourceT m r) ->
  m r
withLayerAndInterceptor interceptor deps (Layer l) useEnv = runResourceT $ do
  lenv <- LayerEnv <$> newMVar HashMap.empty <*> pure interceptor
  env <- l lenv deps
  useEnv env

type Environment = Rec Identity

-- |
-- An empty environment with no services.
-- This is the starting point for building up an environment.
emptyEnvironment :: Environment '[]
emptyEnvironment = RNil

lookupEnvironment :: forall r rs. (r ∈ rs) => Environment rs -> r
lookupEnvironment env = runIdentity $ rget @r env

mergeEnvironment ::
  forall as bs.
  () =>
  Environment as -> Environment bs -> Environment (as ++ bs)
mergeEnvironment = rappend

appendEnvironment :: forall service env. service -> Environment env -> Environment (service ': env)
appendEnvironment x xs = pure x :& xs

class AssembleFromEnvironment env constr where
  assembleFromEnvironment :: Environment env -> constr -> Assembled constr

-- |
-- Type family that unwraps function types to get the final result type.
-- This is used by 'AssembleFromEnvironment' to handle function types
-- for automatic dependency injection.
--
-- Example:
-- @
-- type Result = Assembled (Config -> Database -> Service)
-- -- Result = Service
-- @
type family Assembled a where
  Assembled (_1 -> a) = Assembled a
  Assembled a = a

instance (arg ∈ env, AssembleFromEnvironment env rest) => AssembleFromEnvironment env (arg -> rest) where
  assembleFromEnvironment env f = assembleFromEnvironment env (f $ lookupEnvironment env)

instance {-# OVERLAPPABLE #-} (rest ~ Assembled rest) => AssembleFromEnvironment env rest where
  assembleFromEnvironment _ x = x