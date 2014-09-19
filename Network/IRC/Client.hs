{-# LANGUAGE OverloadedStrings  #-}

-- |Entry point to the client.
module Network.IRC.Client
    ( -- *Initialisation
      connect
    , connectWithTLS
    , start
    , start'
    -- *Interaction
    , send
    , sendBS
    , disconnect
    -- *Defaults
    , defaultIRCConf
    , defaultDisconnectHandler
    , defaultEventHandlers
    -- *Types
    , module Network.IRC.Client.Types
    -- *Utilities
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

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => ByteString -> Int -> NominalDiffTime -> m ConnectionConfig
connect = connect' ircClient defaultDisconnectHandler

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => ByteString -> Int -> NominalDiffTime -> m ConnectionConfig
connectWithTLS = connect' ircTLSClient defaultDisconnectHandler

-- *Starting

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
start :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
start cconf iconf = newIRCState cconf iconf >>= start'

-- |Like 'start', but use the provided initial state.
start' :: MonadIO m => IRCState -> m ()
start' = liftIO . runReaderT runner

-- *Default configuration

-- |Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> InstanceConfig
defaultIRCConf n = InstanceConfig
                     { _nick          = n
                     , _username      = n
                     , _realname      = n
                     , _channels      = []
                     , _ctcpVer       = "irc-client-0.2.1.0"
                     , _eventHandlers = defaultEventHandlers
                     , _ignore        = []
                     }
