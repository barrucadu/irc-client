{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Utils.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes
--
-- Utilities for dealing with lenses without depending on the lens
-- library.
module Network.IRC.Client.Utils.Lens where

import Control.Applicative (Const(..))
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity (Identity(..))

-- | Atomically snapshot some shared state.
--
-- @snapshot :: MonadIO m => (Lens' s (TVar a)) -> s -> m a
snapshot :: MonadIO m => (forall f. Functor f => (TVar a -> f (TVar a)) -> s -> f s) -> s -> m a
snapshot lens = liftIO . atomically . readTVar . get lens

-- | Atomically snapshot and modify some shared state.
--
-- @snapshotModify :: MonadIO m => (Lens' s (TVar a)) -> (a -> STM (a, b)) -> s -> m b@
snapshotModify :: MonadIO m => (forall f. Functor f => (TVar a -> f (TVar a)) -> s -> f s) -> (a -> STM (a, b)) -> s -> m b
snapshotModify lens f s = liftIO . atomically $ do
  let avar = get lens s
  a <- readTVar avar
  (a', b) <- f a
  writeTVar avar a'
  pure b

-- | Get a value from a lens.
--
-- @get :: Lens' s a -> s -> a
get :: (forall f. Functor f => (a -> f a) -> s -> f s) -> s -> a
get lens = getConst . lens Const

-- | Set a value in a lens.
--
-- @set :: Lens' s a -> s -> a -> s@
set :: (forall f. Functor f => (a -> f a) -> s -> f s) -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

-- | Modify a value in a lens.
--
-- @modify :: Lens' s a -> s -> (a -> a) -> s -> s@
modify :: (forall f. Functor f => (a -> f a) -> s -> f s) -> (a -> a) -> s -> s
modify lens f s = let a = get lens s in set lens (f a) s
