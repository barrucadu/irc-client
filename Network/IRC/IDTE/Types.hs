-- |Types for IRC clients, because GHC doesn't do recursive modules
-- well.
module Network.IRC.IDTE.Types
    ( -- *State
      IRC
    , IRCState
    , ConnectionConfig(..)
    , InstanceConfig(..)
    , newIRCState
    , ircState
    , getConnectionConfig
    , getInstanceConfig
    , getInstanceConfig'
    , connectionConfig
    , instanceConfigTVar
    , instanceConfig
    , putInstanceConfig
    , withLock

    -- *Events
    , Event(..)
    , EventType(..)
    , EventHandler(..)
    , Source(..)
    , IrcMessage(..)
    ) where

import Control.Applicative        ((<$>))
import Control.Concurrent.MVar    (MVar, newMVar, withMVar)
import Control.Concurrent.STM     (TVar, atomically, readTVar, newTVar, writeTVar)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Text                  (Text)
import Data.Time.Clock            (UTCTime)
import Network                    (HostName)
import Network.Socket             (Socket)
import Network.IRC                (Message)
import Network.TLS                (Context)

-- *State

-- |The IRC monad: read-only connection configuration, mutable
-- instance configuration in STM, and a lock behind an MVar.
type IRC a = ReaderT IRCState IO a

data IRCState = IRCState { _connectionConfig :: ConnectionConfig
                         -- ^Read-only connection configuration
                         , _instanceConfig   :: TVar InstanceConfig
                         -- ^Mutable instance configuration in STM
                         , _globalLock       :: MVar ()
                         -- ^Lock for atomic operations
                         }

-- |Construct a new IRC state
newIRCState :: MonadIO m => ConnectionConfig -> InstanceConfig -> m IRCState
newIRCState cconf iconf = do
  tvar <- liftIO . atomically . newTVar $ iconf
  mvar <- liftIO . newMVar $ ()

  return IRCState { _connectionConfig = cconf
                  , _instanceConfig   = tvar
                  , _globalLock       = mvar
                  }

-- |Access the entire state.
ircState :: IRC IRCState
ircState = ask

-- |Extract the connection configuration from an IRC state
getConnectionConfig :: IRCState -> ConnectionConfig
getConnectionConfig = _connectionConfig

-- |Extract the instance configuration from an IRC state
getInstanceConfig :: IRCState -> TVar InstanceConfig
getInstanceConfig = _instanceConfig

-- |Extract the current snapshot of the instance configuration from an
-- IRC state
getInstanceConfig' :: MonadIO m => IRCState -> m InstanceConfig
getInstanceConfig' = liftIO . atomically . readTVar . _instanceConfig

-- |Access the connection config
connectionConfig :: IRC ConnectionConfig
connectionConfig = _connectionConfig <$> ask

-- |Access the instance config TVar
instanceConfigTVar :: IRC (TVar InstanceConfig)
instanceConfigTVar = _instanceConfig <$> ask

-- |Access the instance config as it is right now.
instanceConfig :: IRC InstanceConfig
instanceConfig = instanceConfigTVar >>= liftIO . atomically . readTVar

-- |Overwrite the instance config, even if it has changed since we
-- looked at it.
putInstanceConfig :: InstanceConfig -> IRC ()
putInstanceConfig iconf = instanceConfigTVar >>= liftIO . atomically . flip writeTVar iconf

-- |Run a function atomically with respect to the global lock. This
-- blocks until the lock is free.
withLock :: IO a -> IRC a
withLock f = do
  mvar <- _globalLock <$> ask
  liftIO $ withMVar mvar (const f)

-- |The state of an IRC server connection
data ConnectionConfig = ConnectionConfig
    { _socket     :: Socket
    -- ^Server connection socket
    , _tls        :: Maybe Context
    -- ^TLS context, if TLS was used.
    , _server     :: HostName
    -- ^The server host
    , _port       :: Int
    -- ^The server port
    , _disconnect :: IRC ()
    -- ^Action to run if the remote server closes the connection.
    }

-- |The updateable state of an IRC connection
data InstanceConfig = InstanceConfig
    { _nick     :: Text
    -- ^Client nick
    , _username :: Text
    -- ^Client username
    , _realname :: Text
    -- ^Client realname
    , _channels :: [Text]
    -- ^Current channels
    , _ctcpVer  :: Text
    -- ^Response to CTCP VERSION
    , _floodDelay :: Int
    -- ^Number of seconds to wait between sends
    , _lastMessageTime :: UTCTime
    -- ^The time of the last message (will be some time in the past
    -- upon initialisation). This is used for flood control and should
    -- NOT be updated by the user.
    , _eventHandlers :: [EventHandler]
    -- ^The registered event handlers
    }

-- *Events

-- |An event has a message, some information on the source, and a
-- reply function.
data Event = Event
    { _rawMessage :: Message
    -- ^The original message, split into parts and nothing more.
    , _eventType  :: EventType
    -- ^The type of the event, as registered by event handlers.
    , _source     :: Source
    -- ^The source of the message.
    , _message    :: IrcMessage
    -- ^The message data, split into a sum type.
    , _reply      :: IrcMessage -> IRC ()
    -- ^Sends a message to the source of this event.
    , _send       :: Source -> IrcMessage -> IRC ()
    -- ^Send a message
    }

-- |Types of events which can be caught.
data EventType = EEverything
               -- ^Match all events
               | ENothing
               -- ^Match no events
               | EPrivmsg | ENotice | ECTCP | ENick | EJoin | EPart | EQuit | EMode | ETopic | EInvite | EKick | EPing | ENumeric
               deriving (Eq, Show)

-- |A function which handles an event.
data EventHandler = EventHandler
    { _description :: Text
    -- ^A description of the event handler
    , _matchType   :: EventType
    -- ^Which type to be triggered by
    , _eventFunc   :: Event -> IRC ()
    -- ^The function to call
    }

-- |The source of a message.
data Source = Server
            -- ^The message comes from the server.
            | Channel Text Text
            -- ^A channel the client is in. The first Text is the
            -- nick, the second, the channel name.
            | User Text
            -- ^A query from a user.
            | UnknownSource
            -- ^The source could not be determined, see the raw
            -- message
            deriving (Eq, Show)

-- |A decoded message
data IrcMessage = Privmsg Text
                -- ^The client has received a message, which may be to
                -- a channel it's in.
                --
                -- CTCPs will, however, not raise this event (despite
                -- being sent as a PRIVMSG).

                | Notice Text
                -- ^Like a PRIVMSG, except an automatic reply must
                -- *not* be generated.

                | CTCP Text [Text]
                -- ^A CTCP has been received.

                | Nick Text
                -- ^Someone has updated their nick. The given nickname
                -- is the new one.

                | Join Text
                -- ^Someone has joined a channel the client is in.

                | Part Text (Maybe Text)
                -- ^Someone has left a channel the client is in.

                | Quit Text (Maybe Text)
                -- ^Someone has quit a channel the client is in.

                | Mode Bool [Text] [Text]
                -- ^Some mode changes have been applied to a channel
                -- the client is in, or a user in a channel the client
                -- is in.

                | Topic Text
                -- ^The topic of a channel the client is in has been
                -- updated.

                | Invite Text Text
                -- ^The client has been invited to a channel. The
                -- first text is the nick of the inviter.

                | Kick Text (Maybe Text)
                -- ^Someone has been kicked from a channel the client
                -- is in.

                | Ping Text
                -- ^A ping has been received (probably from the
                -- server, due to a period of inactivity). The Text is
                -- where the PONG should be sent.

                | Numeric Int [Text]
                -- ^One of the many numeric codes has been received in
                -- response to something the client did.

                | UnknownMessage
                -- ^The message could not be decoded, see the raw
                -- message.
                deriving (Eq, Show)
