{-# LANGUAGE ImpredicativeTypes #-}

-- |Types for IRC clients, because GHC doesn't do recursive modules
-- well.
module Network.IRC.IDTE.Types
    ( module Network.IRC.IDTE.Types

    -- *Re-exported
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

-- |The IRC monad: read-only connection configuration, mutable
-- instance configuration in STM, and a lock behind an MVar.
type IRC a = ReaderT IRCState IO a

data IRCState = IRCState { _connectionConfig :: ConnectionConfig
                         -- ^Read-only connection configuration
                         , _instanceConfig   :: TVar InstanceConfig
                         -- ^Mutable instance configuration in STM
                         }

-- |Construct a new IRC state
newIRCState :: MonadIO m => ConnectionConfig -> InstanceConfig -> m IRCState
newIRCState cconf iconf = do
  tvar <- liftIO . atomically . newTVar $ iconf

  return IRCState { _connectionConfig = cconf
                  , _instanceConfig   = tvar
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

-- |The state of an IRC server connection
data ConnectionConfig = ConnectionConfig
    { _func       :: Int -> ByteString -> IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()
    -- ^Function to connect and start the conduits.
    , _sendqueue  :: TBMChan IrcMessage
    -- ^Message send queue
    , _server     :: ByteString
    -- ^The server host
    , _port       :: Int
    -- ^The server port
    , _flood      :: NominalDiffTime
    -- ^The minimum time between two adjacent messages.
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
    , _eventHandlers :: [EventHandler]
    -- ^The registered event handlers
    }

-- *Events

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
    , _eventFunc   :: UnicodeEvent -> IRC ()
    -- ^The function to call
    }

-- |Get the type of an event.
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
