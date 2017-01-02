{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Utils.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ImpredicativeTypes, RankNTypes
--
-- Lenses and utilities for dealing with lenses without depending on
-- the lens library.
module Network.IRC.Client.Lens where

import Control.Applicative (Const(..))
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Conduit (Consumer, Producer)
import Data.Conduit.TMChan (TBMChan)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.IRC.Conduit

import Network.IRC.Client.Internal.Types

-- CPP seem to dislike the first ' on the RHS…
#define PRIME() '

#define LENS(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Lens' for '_/**/F'. -}; \
    F :: Lens' S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)


-------------------------------------------------------------------------------
-- * Lenses for 'IRCState'

LENS((IRCState s),connectionConfig,(ConnectionConfig s))
LENS((IRCState s),userState,(TVar s))
LENS((IRCState s),instanceConfig,(TVar (InstanceConfig s)))
LENS((IRCState s),connectionState,(TVar ConnectionState))


-------------------------------------------------------------------------------
-- * Lenses for 'ConnectionConfig'

LENS((ConnectionConfig s),func,(IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()))
LENS((ConnectionConfig s),sendqueue,(TBMChan IrcMessage))
LENS((ConnectionConfig s),server,ByteString)
LENS((ConnectionConfig s),port,Int)
LENS((ConnectionConfig s),username,Text)
LENS((ConnectionConfig s),realname,Text)
LENS((ConnectionConfig s),password,(Maybe Text))
LENS((ConnectionConfig s),flood,NominalDiffTime)
LENS((ConnectionConfig s),timeout,NominalDiffTime)
LENS((ConnectionConfig s),onconnect,(IRC s ()))
LENS((ConnectionConfig s),ondisconnect,(IRC s ()))
LENS((ConnectionConfig s),logfunc,(Origin -> ByteString -> IO ()))


-------------------------------------------------------------------------------
-- * Lenses for 'InstanceConfig'

LENS((InstanceConfig s),nick,Text)
LENS((InstanceConfig s),channels,[Text])
LENS((InstanceConfig s),version,Text)
LENS((InstanceConfig s),handlers,[EventHandler s])
LENS((InstanceConfig s),ignore,[(Text, Maybe Text)])


-------------------------------------------------------------------------------
-- * Lenses for 'EventHandler'

LENS((EventHandler s),eventPred,(Event Text -> Bool))
LENS((EventHandler s),eventFunc,(Event Text -> IRC s ()))


-------------------------------------------------------------------------------
-- * Utilities

-- | Get a value from a lens.
get :: Lens' s a -> s -> a
get lens = getConst . lens Const

-- | Set a value in a lens.
set :: Lens' s a -> a -> s -> s
set lens a = runIdentity . lens (\_ -> Identity a)

-- | Modify a value in a lens.
modify :: Lens' s a -> (a -> a) -> s -> s
modify lens f s = let a = get lens s in set lens (f a) s

-- | Atomically snapshot some shared state.
snapshot :: MonadIO m => Lens' s (TVar a) -> s -> m a
snapshot lens = liftIO . atomically . readTVar . get lens

-- | Atomically snapshot and modify some shared state.
snapshotModify :: MonadIO m => Lens' s (TVar a) -> (a -> STM (a, b)) -> s -> m b
snapshotModify lens f s = liftIO . atomically $ do
  let avar = get lens s
  a <- readTVar avar
  (a', b) <- f a
  writeTVar avar a'
  pure b
