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

  -- * Events
  , EventType(..)
  , eventType

  -- ** Event Handlers
  , EventHandler
  , eventHandler
  , matchCTCP
  , matchNumeric
  , matchType
  , eventPredicate
  , eventFunction

  -- * Miscellaneous
  , Origin(..)
  , UnicodeEvent
  , UnicodeSource
  , UnicodeMessage

  -- * Re-exported
  , Event(..)
  , Source(..)
  , Message(..)
  ) where

import Control.Applicative        ((<$>))
import Control.Arrow              (first)
import Control.Concurrent.STM     (STM, TVar, atomically, readTVar, newTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Text                  (Text, toUpper)
import Network.IRC.CTCP           (fromCTCP)
import Network.IRC.Conduit        (Event(..), Message(..), Source(..))

import Network.IRC.Client.Types.Internal

-------------------------------------------------------------------------------
-- Type synonyms

type UnicodeEvent   = Event Text
type UnicodeSource  = Source Text
type UnicodeMessage = Message Text


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
-- Events

-- | Types of events which can be caught.
data EventType
  = EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | EPong | ENumeric | ERaw
  deriving (Eq, Show)

-- | Construct an event handler.
eventHandler
  :: (UnicodeEvent -> Bool)
  -- ^ Event matching predicate
  -> (UnicodeEvent -> StatefulIRC s ())
  -- ^ Event handler.
  -> EventHandler s
eventHandler = EventHandler

-- | A simple predicate to match events of the given type.
matchType :: EventType -> Event a -> Bool
matchType etype = (==etype) . eventType

-- | Match a numeric reply.
matchNumeric :: [Int] -> Event a -> Bool
matchNumeric nums ev = case _message ev of
  Numeric num _ -> num `elem` nums
  _ -> False

-- | Match a CTCP PRIVMSG.
matchCTCP :: [Text] -> UnicodeEvent -> Bool
matchCTCP verbs ev = case _message ev of
  Privmsg _ (Left ctcpbs) ->
    let (verb, _) = first toUpper $ fromCTCP ctcpbs
    in verb `elem` verbs
  _ -> False

-- | Lens to the matching predicate of an event handler.
--
-- @matchType :: Lens' (EventHandler s) (UnicodeEvent -> Bool)@
eventPredicate :: Functor f => ((UnicodeEvent -> Bool) -> f (UnicodeEvent -> Bool)) -> EventHandler s -> f (EventHandler s)
eventPredicate f h = (\mt' -> h { _eventPred = mt' }) <$> f (_eventPred h)

-- | Lens to the handling function of an event handler.
--
-- @eventFunction :: Lens' (EventHandler s) (UnicodeEvent -> StatefulIRC s ())@
eventFunction :: Functor f => ((UnicodeEvent -> StatefulIRC s ()) -> f (UnicodeEvent -> StatefulIRC s ())) -> EventHandler s -> f (EventHandler s)
eventFunction f h = (\ef' -> h { _eventFunc = ef' }) <$> f (_eventFunc h)

-- | Get the type of an event.
eventType :: Event a -> EventType
eventType e = case _message e of
  (Privmsg _ Right{}) -> EPrivmsg
  (Privmsg _ Left{})  -> ECTCP
  (Notice  _ Right{}) -> ENotice
  (Notice  _ Left{})  -> ECTCP

  Nick{}    -> ENick
  Join{}    -> EJoin
  Part{}    -> EPart
  Quit{}    -> EQuit
  Mode{}    -> EMode
  Topic{}   -> ETopic
  Invite{}  -> EInvite
  Kick{}    -> EKick
  Ping{}    -> EPing
  Pong{}    -> EPong
  Numeric{} -> ENumeric
  RawMsg{}  -> ERaw
