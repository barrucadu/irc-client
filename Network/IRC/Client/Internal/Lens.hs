{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Internal.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ImpredicativeTypes
--
-- Types and functions for dealing with optics without depending on
-- the lens library.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Internal.Lens where

import           Control.Applicative        (Const(..))
import           Control.Concurrent.STM     (STM, TVar, atomically, readTVar,
                                             readTVarIO, writeTVar)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Functor.Contravariant (Contravariant)
import           Data.Functor.Identity      (Identity(..))
import           Data.Monoid                (First(..))
import           Data.Profunctor            (Choice)


-------------------------------------------------------------------------------
-- * Internal lens synonyms

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Control.Lens.Lens.Lens>@.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Lens'.
type Lens' s a = Lens s s a a

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getter Control.Lens.Getter.Getter>@.
type Getter s a = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Getter.html#t:Getting Control.Lens.Getter.Getting>@.
type Getting r s a = (a -> Const r a) -> s -> Const r s

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Prism.html#t:Prism Control.Lens.Prism.Prism>@.
type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Prism'.
type Prism' s a = Prism s s a a


-------------------------------------------------------------------------------
-- * Utilities

-- | Get a value from a lens.
{-# INLINE get #-}
get :: Getting a s a -> s -> a
get lens = getConst . lens Const

-- | Set a value in a lens.
{-# INLINE set #-}
set :: Lens' s a -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

-- | Modify a value in a lens.
{-# INLINE modify #-}
modify :: Lens' s a -> (a -> a) -> s -> s
modify lens f s = let a = get lens s in set lens (f a) s

-- | Read a value from a prism.
{-# INLINE preview #-}
preview :: Prism' s a -> s -> Maybe a
preview lens = getFirst . getConst . lens (Const . First . Just)


-------------------------------------------------------------------------------
-- ** STM

-- | Atomically snapshot some shared state.
snapshot :: MonadIO m => Getting (TVar a) s (TVar a) -> s -> m a
snapshot lens = liftIO . readTVarIO . get lens

-- | Atomically snapshot and modify some shared state.
snapshotModify :: MonadIO m => Lens' s (TVar a) -> (a -> STM (a, b)) -> s -> m b
snapshotModify lens f s = liftIO . atomically $ do
  let avar = get lens s
  a <- readTVar avar
  (a', b) <- f a
  writeTVar avar a'
  pure b
