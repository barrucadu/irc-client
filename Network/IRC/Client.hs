{-# LANGUAGE OverloadedStrings  #-}

-- |
-- Module      : Network.IRC.Client
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- A simple IRC client library. Typical usage will be of this form:
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
  , connectWithTLSConfig
  , connectWithTLSVerify
  , start
  , start'
  , startStateful

  -- * Logging
  , Origin (..)
  , connect'
  , connectWithTLS'
  , connectWithTLSConfig'
  , connectWithTLSVerify'
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
  , C.rawMessage
  , C.toByteString
  ) where

import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString            (ByteString)
import qualified Data.Conduit.Network.TLS as TLS
import Data.Text                  (Text)
import Data.Time.Clock            (NominalDiffTime)
import qualified Data.X509 as X
import qualified Data.X509.CertificateStore as X
import qualified Data.X509.Validation as X
import qualified Network.Connection as TLS (TLSSettings(..))
import Network.IRC.Client.Handlers
import Network.IRC.Client.Internal
import Network.IRC.Client.Types
import Network.IRC.Client.Utils
import qualified Network.IRC.Conduit as C
import qualified Network.TLS as TLS

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

-- | Connect to a server with TLS using the given TLS config.
connectWithTLSConfig :: MonadIO m
  => TLS.TLSClientConfig
  -- ^ The TLS config
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLSConfig = connectWithTLSConfig' noopLogger

-- | Connect to a server with TLS using the given certificate
-- verifier.
connectWithTLSVerify :: MonadIO m
  => (X.CertificateStore -> TLS.ValidationCache -> X.ServiceID -> X.CertificateChain -> IO [X.FailedReason])
  -- ^ The certificate verifier. Returns an empty list if the cert is
  -- good.
  -> ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLSVerify = connectWithTLSVerify' noopLogger

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
connect' lg host port =
  connectInternal (C.ircClient port host) defaultOnConnect defaultOnDisconnect lg host port

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
connectWithTLS' lg host port =
  connectInternal (C.ircTLSClient port host) defaultOnConnect defaultOnDisconnect lg host port

-- | Connect to a server with TLS using the given TLS config, with the
-- provided logging function.
connectWithTLSConfig' :: MonadIO m
  => (Origin -> ByteString -> IO ())
  -- ^ The message logger
  -> TLS.TLSClientConfig
  -- ^ The TLS config
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLSConfig' lg cfg =
  connectInternal (C.ircTLSClient' cfg) defaultOnConnect defaultOnDisconnect lg host port
  where
    host = TLS.tlsClientHost cfg
    port = TLS.tlsClientPort cfg

-- | Connect to a server with TLS using the given certificate
-- verifier, with the provided logging function.
connectWithTLSVerify' :: MonadIO m
  => (Origin -> ByteString -> IO ())
  -- ^ The message logger
  -> (X.CertificateStore -> TLS.ValidationCache -> X.ServiceID -> X.CertificateChain -> IO [X.FailedReason])
  -- ^ The certificate verifier. Returns an empty list if the cert is
  -- good.
  -> ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> NominalDiffTime
  -- ^ The flood cooldown
  -> m (ConnectionConfig s)
connectWithTLSVerify' lg verifier host port =
  connectInternal (C.ircTLSClient' cfg) defaultOnConnect defaultOnDisconnect lg host port
  where
    cfg =
      let cfg0 = C.defaultTLSConfig port host
          -- this is a partial pattern match, but because I'm the
          -- author of irc-conduit I can do this.
          TLS.TLSSettings cTLSSettings = TLS.tlsClientTLSSettings cfg0
          cHooks = TLS.clientHooks cTLSSettings
      in cfg0 { TLS.tlsClientTLSSettings = TLS.TLSSettings cTLSSettings
                { TLS.clientHooks = cHooks
                  { TLS.onServerCertificate = verifier }
                }
              }

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
  , _ctcpVer       = "irc-client-0.4.3"
  , _eventHandlers = defaultEventHandlers
  , _ignore        = []
  }
