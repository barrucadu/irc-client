{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Types
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : RankNTypes
---
-- Types and lenses for IRC clients. See also
-- <http://hackage.haskell.org/package/irc-conduit/docs/Network-IRC-Conduit.html Network.IRC.Conduit> and
-- <http://hackage.haskell.org/package/irc-ctcp/docs/Network-IRC-CTCP.html Network.IRC.CTCP>.
module Network.IRC.Client.Types
  ( -- * The IRC monad
    IRC
  , StatefulIRC
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
  , instanceConfig
  , userState
  , onconnect
  , ondisconnect
  , nick
  , username
  , realname
  , password
  , channels
  , version
  , handlers
  , ignore

  -- * Miscellaneous
  , EventHandler
  , EventType(..)
  , Origin(..)

  -- * Re-exports
  , Event(..)
  , Message(..)
  , Source(..)
  ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.STM     (STM, TVar, atomically, readTVar, newTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text                  (Text)
import Network.IRC.Conduit        (Event(..), Message(..), Source(..))

import Network.IRC.Client.Types.Internal

-------------------------------------------------------------------------------
-- The IRC monad

-- | The IRC monad.
type IRC a = StatefulIRC () a

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
getIrcState :: StatefulIRC s (IRCState s)
getIrcState = ask

-- | Get the connection state from an IRC state.
getConnectionState :: IRCState s -> STM ConnectionState
getConnectionState = readTVar . _connectionState


-------------------------------------------------------------------------------
-- State Lenses

-- | Lens to the instance configuration from an IRC state.
--
-- @instanceConfig :: Lens' (IRCState s) (TVar (InstanceConfig s))@
instanceConfig :: Functor f => (TVar (InstanceConfig s) -> f (TVar (InstanceConfig s))) -> IRCState s -> f (IRCState s)
instanceConfig f s = (\ic' -> s { _instanceConfig = ic' }) <$> f (_instanceConfig s)

-- | Lens to the user state from an IRC state.
--
-- @userState :: Lens' (IRCState s) (TVar s)@
userState :: Functor f => (TVar s -> f (TVar s)) -> IRCState s -> f (IRCState s)
userState f s = (\us' -> s { _userState = us' }) <$> f (_userState s)

-- | Lens to the action to run after connecting to the server. This is
-- run after sending the `PASS` and `USER` commands to the server. The
-- default behaviour is to send the `NICK` command.
--
-- @onconnect :: Lens' (ConnectionConfig s) (StatefulIRC s ())@
onconnect :: Functor f => (StatefulIRC s () -> f (StatefulIRC s ())) -> ConnectionConfig s -> f (ConnectionConfig s)
onconnect f cc = (\oc' -> cc { _onconnect = oc' }) <$> f (_onconnect cc)

-- | Lens to the action to run after disconnecting from the server,
-- both by local choice and by losing the connection. This is run
-- after tearing down the connection. The default behaviour is to do
-- nothing.
--
-- @ondisconnect :: Lens' (ConnectionConfig s) (StatefulIRC s ())@
ondisconnect :: Functor f => (StatefulIRC s () -> f (StatefulIRC s ())) -> ConnectionConfig s -> f (ConnectionConfig s)
ondisconnect f cc = (\od' -> cc { _ondisconnect = od' }) <$> f (_ondisconnect cc)

-- | Lens to the nick from the instance config.
--
-- @nick :: Lens' (InstanceConfig s) Text@
nick :: Functor f => (Text -> f Text) -> InstanceConfig s -> f (InstanceConfig s)
nick f ic = (\n' -> ic { _nick = n' }) <$> f (_nick ic)

-- | Lens to the username from the instance config. The username is
-- sent to the server during the initial set-up.
--
-- @username :: Lens' (InstanceConfig s) Text@
username :: Functor f => (Text -> f Text) -> InstanceConfig s -> f (InstanceConfig s)
username f ic = (\u' -> ic { _username = u' }) <$> f (_username ic)

-- | Lens to the realname from the instance config. The realname is
-- sent to the server during the initial set-up.
--
-- @realname :: Lens' (InstanceConfig s) Text@
realname :: Functor f => (Text -> f Text) -> InstanceConfig s -> f (InstanceConfig s)
realname f ic = (\r' -> ic { _realname = r' }) <$> f (_realname ic)

-- | Lens to the password nick from the instance config. The password
-- is sent to the server during the initial set-up.
--
-- @password :: Lens' (InstanceConfig s) (Maybe Text)@
password :: Functor f => (Maybe Text -> f (Maybe Text)) -> InstanceConfig s -> f (InstanceConfig s)
password f ic = (\p' -> ic { _password = p' }) <$> f (_password ic)

-- | Lens to the channels from the instance config. This list both
-- determines the channels to join on connect, and is modified by the
-- default event handlers when channels are joined or parted.
--
-- @channels :: Lens' (InstanceConfig s) [Text]@
channels :: Functor f => ([Text] -> f [Text]) -> InstanceConfig s -> f (InstanceConfig s)
channels f ic = (\cs' -> ic { _channels = cs' }) <$> f (_channels ic)

-- | Lens to the version from the instance config. The version is sent
-- in response to the CTCP \"VERSION\" request by the default event
-- handlers.
--
-- @version :: Lens' (InstanceConfig s) Text@
version :: Functor f => (Text -> f Text) -> InstanceConfig s -> f (InstanceConfig s)
version f ic = (\v' -> ic { _version = v' }) <$> f (_version ic)

-- | Lens to the event handlers from the instance config.
--
-- @handlers :: Lens' (InstanceConfig s) [EventHandler s]@
handlers :: Functor f => ([EventHandler s] -> f [EventHandler s]) -> InstanceConfig s -> f (InstanceConfig s)
handlers f ic = (\n' -> ic { _handlers = n' }) <$> f (_handlers ic)

-- | Lens to the ignore list from the instance config. This is a list
-- of nicks with optional channel restrictions.
--
-- @ignore :: Lens' (InstanceConfig s) [(Text, Maybe Text)]@
ignore :: Functor f => ([(Text, Maybe Text)] -> f [(Text, Maybe Text)]) -> InstanceConfig s -> f (InstanceConfig s)
ignore f ic = (\is' -> ic { _ignore = is' }) <$> f (_ignore ic)


-------------------------------------------------------------------------------
-- Miscellaneous

-- | Types of events which can be caught.
data EventType
  = EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | EPong | ENumeric | ERaw
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
