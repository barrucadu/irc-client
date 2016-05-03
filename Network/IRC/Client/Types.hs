{-# LANGUAGE ImpredicativeTypes #-}

-- | Types for IRC clients. See also
-- <http://hackage.haskell.org/package/irc-conduit/docs/Network-IRC-Conduit.html Network.IRC.Conduit> and
-- <http://hackage.haskell.org/package/irc-ctcp-0.1.2.1/docs/Network-IRC-CTCP.html Network.IRC.CTCP>.
module Network.IRC.Client.Types
  ( module Network.IRC.Client.Types

  -- * Re-exported
  , Event(..)
  , Source(..)
  , Message(..)
  ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.STM     (TVar, atomically, readTVar, newTVar, writeTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.ByteString            (ByteString)
import Data.Conduit               (Consumer, Producer)
import Data.Conduit.TMChan        (TBMChan)
import Data.Text                  (Text)
import Data.Time.Clock            (NominalDiffTime)
import Network.IRC.Conduit        (Event(..), Message(..), Source(..), IrcEvent, IrcMessage)

-- *Type synonyms
type UnicodeEvent   = Event Text
type UnicodeSource  = Source Text
type UnicodeMessage = Message Text

-- *State

-- | The IRC monad.
type IRC a = StatefulIRC () a

-- | The IRC monad, with state.
type StatefulIRC s a = ReaderT (IRCState s) IO a

data IRCState s = IRCState { _connectionConfig :: ConnectionConfig s
                           -- ^Read-only connection configuration
                           , _userState :: TVar s
                           -- ^Mutable user state
                           , _instanceConfig   :: TVar (InstanceConfig s)
                           -- ^Mutable instance configuration in STM
                           }

-- | Construct a new IRC state
newIRCState :: MonadIO m => ConnectionConfig s -> InstanceConfig s -> s -> m (IRCState s)
newIRCState cconf iconf ustate = do
  ustvar <- liftIO . atomically . newTVar $ ustate
  ictvar <- liftIO . atomically . newTVar $ iconf

  return IRCState
    { _connectionConfig = cconf
    , _userState        = ustvar
    , _instanceConfig   = ictvar
    }

-- | Access the client state.
ircState :: StatefulIRC s (IRCState s)
ircState = ask

-- | Extract the connection configuration from an IRC state
getConnectionConfig :: IRCState s -> ConnectionConfig s
getConnectionConfig = _connectionConfig

-- | Extract the instance configuration from an IRC state
getInstanceConfig :: IRCState s -> TVar (InstanceConfig s)
getInstanceConfig = _instanceConfig

-- | Extract the user state from an IRC state
getUserState :: IRCState s -> TVar s
getUserState = _userState

-- | Extract the current snapshot of the instance configuration from
-- an IRC state
getInstanceConfig' :: MonadIO m => IRCState s -> m (InstanceConfig s)
getInstanceConfig' = liftIO . atomically . readTVar . _instanceConfig

-- | Access the connection config
connectionConfig :: StatefulIRC s (ConnectionConfig s)
connectionConfig = _connectionConfig <$> ask

-- | Access the instance config TVar
instanceConfigTVar :: StatefulIRC s (TVar (InstanceConfig s))
instanceConfigTVar = _instanceConfig <$> ask

-- | Access the instance config as it is right now.
instanceConfig :: StatefulIRC s (InstanceConfig s)
instanceConfig = instanceConfigTVar >>= liftIO . atomically . readTVar

-- | Overwrite the instance config, even if it has changed since we
-- looked at it.
putInstanceConfig :: InstanceConfig s -> StatefulIRC s ()
putInstanceConfig iconf = instanceConfigTVar >>= liftIO . atomically . flip writeTVar iconf

-- | Access the user state.
stateTVar :: StatefulIRC s (TVar s)
stateTVar = _userState <$> ask

-- | Access the user state as it is right now.
state :: StatefulIRC s s
state = stateTVar >>= liftIO . atomically . readTVar

-- | Set the user state.
putState :: s -> StatefulIRC s ()
putState s = stateTVar >>= liftIO . atomically . flip writeTVar s

-- | The origin of a message.
data Origin = FromServer | FromClient
  deriving (Eq, Read, Show)

-- | The static state of an IRC server connection.
data ConnectionConfig s = ConnectionConfig
  { _func       :: Int -> ByteString -> IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()
  -- ^ Function to connect and start the conduits.
  , _sendqueue  :: TBMChan IrcMessage
  -- ^ Message send queue.
  , _server     :: ByteString
  -- ^ The server host.
  , _port       :: Int
  -- ^ The server port.
  , _flood      :: NominalDiffTime
  -- ^ The minimum time between two adjacent messages.
  , _onconnect :: StatefulIRC s ()
  -- ^ Action to run after successfully connecting to the server and
  -- setting the nick.
  , _ondisconnect :: StatefulIRC s ()
  -- ^ Action to run if the remote server closes the connection.
  , _logfunc    :: Origin -> ByteString -> IO ()
  -- ^ Function to log messages sent to and received from the server.
  }

-- | The updateable state of an IRC connection.
data InstanceConfig s = InstanceConfig
  { _nick     :: Text
  -- ^ Client nick
  , _username :: Text
  -- ^ Client username
  , _realname :: Text
  -- ^ Client realname
  , _channels :: [Text]
  -- ^ Current channels
  , _ctcpVer  :: Text
  -- ^ Response to CTCP VERSION
  , _eventHandlers :: [EventHandler s]
  -- ^ The registered event handlers
  , _ignore   :: [(Text, Maybe Text)]
  -- ^ List of nicks (optionally restricted to channels) to ignore
  -- messages from. No channel = global.
  }

-- *Events

-- | Types of events which can be caught.
data EventType
  = EEverything
  -- ^ Match all events
  | ENothing
  -- ^ Match no events
  | EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | ENumeric
  deriving (Eq, Show)

-- | A function which handles an event.
data EventHandler s = EventHandler
  { _description :: Text
  -- ^ A description of the event handler.
  , _matchType   :: EventType
  -- ^ Which type to be triggered by
  , _eventFunc   :: UnicodeEvent -> StatefulIRC s ()
  -- ^ The function to call.
  }

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
  Numeric{} -> ENumeric

  _ -> EEverything
