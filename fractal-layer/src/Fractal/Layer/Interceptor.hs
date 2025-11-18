{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Fractal.Layer.Interceptor
-- Description : Interceptor interface for instrumenting layer operations
-- Stability   : experimental
-- Portability : Portable
--
-- This module provides an extensible interceptor mechanism for observing
-- and instrumenting layer operations without modifying core layer code.
--
-- The interceptor pattern allows plugging in different behaviors like:
-- - Diagnostics and visualization
-- - Logging
-- - Metrics collection
-- - Distributed tracing
-- - Performance profiling
--
-- == Example: Logging Interceptor
--
-- @
-- loggingInterceptor :: LayerInterceptor IO
-- loggingInterceptor = LayerInterceptor
--   { onResourceAcquire = \name _ -> putStrLn $ "Acquiring: " <> T.unpack name
--   , onResourceRelease = \name duration ->
--       putStrLn $ "Released: " <> T.unpack name <> " (" <> show duration <> "s)"
--   , onEffectRun = \name _ -> putStrLn $ "Running effect: " <> T.unpack name
--   , onServiceCreate = \name _ -> putStrLn $ "Creating service: " <> T.unpack name
--   , onServiceReuse = \name -> putStrLn $ "Reusing service: " <> T.unpack name
--   , onCompositionStart = \typ -> putStrLn $ "Composing: " <> show typ
--   , onCompositionEnd = \_ _ -> pure ()
--   }
-- @
module Fractal.Layer.Interceptor
  ( -- * Core Interface
    LayerInterceptor (..),
    CompositionType (..),
    OperationContext (..),

    -- * Built-in Interceptors
    nullInterceptor,
    combineInterceptors,
  )
where

import Control.Monad (void)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Data.Typeable (TypeRep)
import UnliftIO (MonadIO, liftIO)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- | Type of layer composition
data CompositionType
  = Sequential
  -- ^ Sequential composition (>>>)
  | Parallel
  -- ^ Parallel composition (&&&)
  deriving (Show, Eq)

-- | Context information about an operation
data OperationContext = OperationContext
  { operationName :: !Text
  -- ^ Human-readable name of the operation
  , operationType :: !(Maybe TypeRep)
  -- ^ Type representation if available
  , operationMetadata :: ![(Text, Text)]
  -- ^ Additional metadata as key-value pairs
  }
  deriving (Show)

-- | Interceptor interface for instrumenting layer operations
--
-- Each callback is called at specific points during layer initialization:
--
-- * 'onResourceAcquire' - Before acquiring a resource
-- * 'onResourceRelease' - After releasing a resource
-- * 'onEffectRun' - Before running an effect
-- * 'onEffectComplete' - After an effect completes
-- * 'onServiceCreate' - When creating a new service
-- * 'onServiceReuse' - When reusing a cached service
-- * 'onCompositionStart' - When starting a composition
-- * 'onCompositionEnd' - When completing a composition
--
-- All callbacks receive context information and timing data where applicable.
data LayerInterceptor m = LayerInterceptor
  { onResourceAcquire :: OperationContext -> m ()
  -- ^ Called before acquiring a resource
  , onResourceRelease :: Text -> NominalDiffTime -> m ()
  -- ^ Called after releasing a resource (with operation name and duration)
  , onEffectRun :: OperationContext -> m ()
  -- ^ Called before running an effect
  , onEffectComplete :: Text -> NominalDiffTime -> m ()
  -- ^ Called after an effect completes
  , onServiceCreate :: OperationContext -> m ()
  -- ^ Called when creating a new service
  , onServiceReuse :: Text -> TypeRep -> m ()
  -- ^ Called when reusing a cached service
  , onCompositionStart :: CompositionType -> m ()
  -- ^ Called when starting a composition
  , onCompositionEnd :: CompositionType -> NominalDiffTime -> m ()
  -- ^ Called when completing a composition
  }

-------------------------------------------------------------------------------
-- Built-in Interceptors
-------------------------------------------------------------------------------

-- | Null interceptor that does nothing
--
-- This is the default interceptor when none is specified.
-- It has zero overhead as all callbacks are no-ops.
nullInterceptor :: Applicative m => LayerInterceptor m
nullInterceptor =
  LayerInterceptor
    { onResourceAcquire = \_ -> pure ()
    , onResourceRelease = \_ _ -> pure ()
    , onEffectRun = \_ -> pure ()
    , onEffectComplete = \_ _ -> pure ()
    , onServiceCreate = \_ -> pure ()
    , onServiceReuse = \_ _ -> pure ()
    , onCompositionStart = \_ -> pure ()
    , onCompositionEnd = \_ _ -> pure ()
    }

-- | Combine multiple interceptors into one
--
-- Callbacks are executed in the order the interceptors are provided.
-- If any callback throws an exception, subsequent interceptors are not called.
--
-- Example:
-- @
-- combined = combineInterceptors
--   [ loggingInterceptor
--   , metricsInterceptor
--   , tracingInterceptor
--   ]
-- @
combineInterceptors :: Monad m => [LayerInterceptor m] -> LayerInterceptor m
combineInterceptors interceptors =
  LayerInterceptor
    { onResourceAcquire = \ctx -> mapM_ (\i -> onResourceAcquire i ctx) interceptors
    , onResourceRelease = \name dur -> mapM_ (\i -> onResourceRelease i name dur) interceptors
    , onEffectRun = \ctx -> mapM_ (\i -> onEffectRun i ctx) interceptors
    , onEffectComplete = \name dur -> mapM_ (\i -> onEffectComplete i name dur) interceptors
    , onServiceCreate = \ctx -> mapM_ (\i -> onServiceCreate i ctx) interceptors
    , onServiceReuse = \name tr -> mapM_ (\i -> onServiceReuse i name tr) interceptors
    , onCompositionStart = \typ -> mapM_ (\i -> onCompositionStart i typ) interceptors
    , onCompositionEnd = \typ dur -> mapM_ (\i -> onCompositionEnd i typ dur) interceptors
    }

-------------------------------------------------------------------------------
-- Helper Functions
-------------------------------------------------------------------------------

-- | Create a simple operation context with just a name
simpleContext :: Text -> OperationContext
simpleContext name =
  OperationContext
    { operationName = name
    , operationType = Nothing
    , operationMetadata = []
    }

-- | Add type information to a context
withType :: TypeRep -> OperationContext -> OperationContext
withType tr ctx = ctx {operationType = Just tr}

-- | Add metadata to a context
withMetadata :: [(Text, Text)] -> OperationContext -> OperationContext
withMetadata meta ctx = ctx {operationMetadata = operationMetadata ctx ++ meta}
