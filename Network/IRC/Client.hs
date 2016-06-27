{-# LANGUAGE OverloadedStrings  #-}

-- | A simple IRC client library. Typical usage will be of this form:
--
-- > run :: ByteString -> Int -> Text -> IO ()
-- > run host port nick = do
-- >   conn <- connect host port 1
-- >   let cfg = defaultIRCConf nick
-- >   let cfg' = cfg { _eventHandlers = yourCustomEventHandlers : _eventHandlers cfg }
-- >   start conn cfg'
--
-- You shouldn't really need to tweak anything other than the event
-- handlers, as everything has been designed to be as simple as
-- possible.
module Network.IRC.Client
  ( -- * Initialisation
    connect
  , connectWithTLS
  , start
  , start'
  , startStateful

  -- * Logging
  , Origin (..)
  , connect'
  , connectWithTLS'
  , stdoutLogger
  , fileLogger
  , noopLogger

  -- * Interaction
  , send
  , sendBS
  , disconnect

  -- * Defaults
  , defaultIRCConf
  , defaultOnConnect
  , defaultOnDisconnect
  , defaultEventHandlers

  -- * Types
  , module Network.IRC.Client.Types

  -- * Utilities
  , module Network.IRC.Client.Utils
  , rawMessage
  , toByteString
  ) where

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString            (ByteString)
import Data.Text                  (Text)
import Data.Time.Clock            (NominalDiffTime)
import Network.IRC.Client.Handlers
import Network.IRC.Client.Internal
import Network.IRC.Client.Types
import Network.IRC.Client.Utils
import Network.IRC.Conduit        (ircClient, ircTLSClient, rawMessage, toByteString)

-- * Connecting to an IRC network

-- | Connect to a server without TLS.
connect :: MonadIO m
  => ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connect = connect' noopLogger

-- | Connect to a server with TLS.
connectWithTLS :: MonadIO m
  => ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLS = connectWithTLS' noopLogger

-- | Connect to a server without TLS, with the provided logging
-- function.
connect' :: MonadIO m
  => (Origin -> ByteString -> IO ())
  -- ^ The message logger
  -> ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connect' = connectInternal ircClient defaultOnConnect defaultOnDisconnect

-- | Connect to a server with TLS, with the provided logging function.
connectWithTLS' :: MonadIO m
  => (Origin -> ByteString -> IO ())
  -- ^ The message logger
  -> ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLS' = connectInternal ircTLSClient defaultOnConnect defaultOnDisconnect

-- * Starting

-- | Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate. Messages will be logged to
-- stdout.
start :: MonadIO m => ConnectionConfig () -> InstanceConfig () -> m ()
start cconf iconf = startStateful cconf iconf ()

-- | like 'start' but for clients with state.
startStateful :: MonadIO m => ConnectionConfig s -> InstanceConfig s -> s -> m ()
startStateful cconf iconf ustate = newIRCState cconf iconf ustate >>= start'

-- | Like 'start', but use the provided initial state.
start' :: MonadIO m => IRCState s -> m ()
start' = liftIO . runReaderT runner

-- * Default configuration

-- | Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> InstanceConfig s
defaultIRCConf n = InstanceConfig
  { _nick          = n
  , _username      = n
  , _realname      = n
  , _password      = Nothing
  , _channels      = []
  , _ctcpVer       = "irc-client-0.4.2.0"
  , _eventHandlers = defaultEventHandlers
  , _ignore        = []
  }
