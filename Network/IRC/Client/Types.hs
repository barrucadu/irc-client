-- |
-- Module      : Network.IRC.Client.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
---
-- Types and lenses for IRC clients. See also
-- <http://hackage.haskell.org/package/irc-conduit/docs/Network-IRC-Conduit.html Network.IRC.Conduit> and
-- <http://hackage.haskell.org/package/irc-ctcp/docs/Network-IRC-CTCP.html Network.IRC.CTCP>.
module Network.IRC.Client.Types
  ( -- * The IRC monad
    IRC(..)
  , getIrcState

  -- * State
  , IRCState
  , ConnectionState(..)
  , ConnectionConfig
  , InstanceConfig
  , newIRCState

  -- ** Getters
  , getConnectionState

  -- ** Lenses
  , module Network.IRC.Client.Lens

  -- * Miscellaneous
  , EventHandler
  , EventType(..)
  , Origin(..)
  , Timeout(..)

  -- * Re-exports
  , Event(..)
  , Message(..)
  , Source(..)
  ) where

import Control.Concurrent.STM (STM, atomically, readTVar, newTVar)
import Control.Exception      (Exception)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (ask)
import Network.IRC.Conduit    (Event(..), Message(..), Source(..))

import Network.IRC.Client.Lens
import Network.IRC.Client.Types.Internal

-------------------------------------------------------------------------------
-- State

-- | Construct a new IRC state
newIRCState :: MonadIO m => ConnectionConfig s -> InstanceConfig s -> s -> m (IRCState s)
newIRCState cconf iconf ustate = do
  ustvar <- liftIO . atomically . newTVar $ ustate
  ictvar <- liftIO . atomically . newTVar $ iconf
  cstvar <- liftIO . atomically . newTVar $ Disconnected

  pure IRCState
    { _connectionConfig = cconf
    , _userState        = ustvar
    , _instanceConfig   = ictvar
    , _connectionState  = cstvar
    }


-------------------------------------------------------------------------------
-- State Getters

-- | Access the client state.
getIrcState :: IRC s (IRCState s)
getIrcState = ask

-- | Get the connection state from an IRC state.
getConnectionState :: IRCState s -> STM ConnectionState
getConnectionState = readTVar . _connectionState


-------------------------------------------------------------------------------
-- Miscellaneous

-- | Types of events which can be caught.
data EventType
  = EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | EPong | ENumeric | ERaw
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Exception thrown to kill the client if the timeout elapses with
-- nothing received from the server.
data Timeout = Timeout
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception Timeout
