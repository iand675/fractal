{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Fractal.Layer.Interpreter
-- Description : Interpreter for the Layer free monad
-- Copyright   : (c) Mercury, 2024
-- License     : AllRightsReserved
-- Stability   : experimental
-- Portability : Portable
--
-- This module provides the default interpreter for the Layer free monad,
-- implementing the current behavior with resource management and service caching.
module Fractal.Layer.Interpreter
  ( interpretLayer
  , LayerEnv(..)
  , ServiceState(..)
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Resource
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Typeable
import UnliftIO
import Unsafe.Coerce

import Fractal.Layer.Free

data ServiceState a
  = Initialized a
  | Failed SomeException

data LayerEnv = LayerEnv
  { serviceStates :: !(MVar (HashMap TypeRep (ServiceState Any)))
  }

-- | Interpret a Layer into the current implementation
interpretLayer :: MonadUnliftIO m => LayerEnv -> deps -> Layer m deps env -> ResourceT m env
interpretLayer lenv deps = foldFree $ \case
  Pure x -> pure x

  Resource acq rel next -> do
    (_, env) <- allocateU (lift $ acq deps) (lift . rel)
    pure $ next env

  Effect eff next -> do
    env <- lift $ eff deps
    pure $ next env

  Service eff next -> do
    let rep = typeRep (Proxy @env)

    -- Try to get the current state
    states <- liftIO $ readMVar lenv.serviceStates
    case HashMap.lookup rep states of
      Just (Initialized service) -> pure $ next $ unsafeCoerce service
      Just (Failed e) -> throwIO e
      _ -> join $ liftIO $ modifyMVar lenv.serviceStates $ \states -> do
        case HashMap.lookup rep states of
          Just (Initialized service) -> pure (states, pure $ next $ unsafeCoerce service)
          Just (Failed e) -> pure (states, throwIO e)
          Nothing -> do
            -- Initialize the service
            eRes <- try $ eff deps
            case eRes of
              Left e -> do
                let states' = HashMap.insert rep (Failed e) states
                pure (states', throwIO e)
              Right x -> do
                let states' = HashMap.insert rep (Initialized $ unsafeCoerce x) states
                pure (states', pure $ next x)
