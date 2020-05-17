{-# LANGUAGE OverloadedStrings #-}

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
-- >   let conn = plainConnection host port
-- >   let cfg  = defaultInstanceConfig nick & handlers %~ (yourCustomEventHandlers:)
-- >   runClient conn cfg ()
--
-- You shouldn't really need to tweak anything other than the event
-- handlers, as everything has been designed to be as simple as
-- possible.
module Network.IRC.Client
  ( -- * Configuration

  -- | The configuration is logically split into two parts: the
  -- /connection/ configuration (the 'ConnectionConfig' type) and the
  -- /instance/ configuration (the 'InstanceConfig' type).
  --
  --     - Connection configuration details how to connect to the IRC
  --       server, and cannot be modified after the client has started
  --       (although it can be read).
  --
  --     - Instance configuration is everything else: the client's
  --       nick, and version, handlers for received messages, and so
  --       on. It can be modified after the client has started.

  -- ** Connection configuration

  -- | The following values can be changed with the exported lenses:
  --
  --    - 'username' (default: \"irc-client\"). The username sent to
  --      the server in the \"USER\" command.
  --
  --    - 'realname' (default: \"irc-client\"). The real name sent to
  --      the server in the \"USER\" command.
  --
  --    - 'password' (default: @Nothing@). If set, the password sent to the
  --      server in the \"PASS\" command.
  --
  --    - 'flood' (default: @1@). The minimum time between sending
  --      messages, to avoid flooding.
  --
  --    - 'timeout' (default: @300@). The amount of time to wait for a
  --      message from the server before locally timing out.
  --
  --    - 'onconnect' (default: 'defaultOnConnect'). The action to
  --      perform after sending the \"USER\" and \"PASS\" commands.
  --
  --    - 'ondisconnect' (default: 'defaultOnDisconnect'). The action
  --      to perform after disconnecting from the server
  --
  --    - 'logfunc' (default: 'noopLogger'). The function to log received
  --      and sent messages.

    ConnectionConfig
  , plainConnection
  , TLSConfig(..)
  , tlsConnection

  -- *** Logging

  -- | The logging functions are told whether the message came from
  -- the server or the client, and are given the raw bytestring.

  , Origin(..)
  , stdoutLogger
  , fileLogger
  , noopLogger

  -- ** Instance configuration

  -- | The following values can be changed with the exported lenses:
  --
  --    - 'nick'. The nick that 'defaultOnConnect' sends to the
  --      server. This is also modified during runtime by the
  --      'welcomeNick' and 'nickMangler' default event handlers.
  --
  --    - 'channels' (default: @[]@). The channels that
  --      'joinOnWelcome' joins. This is also modified during runtime
  --      by the 'joinHandler' default event handler.
  --
  --    - 'version' (default: \"irc-client-$VERSION\"). The
  --      version that 'ctcpVersionHandler' sends.
  --
  --    - 'handlers' (default: 'defaultEventHandlers'). The list of
  --      event handlers.
  --
  --    - 'ignore' (default: @[]@). The ignore list, events from
  --      matching nicks are not handled.

  , InstanceConfig
  , defaultInstanceConfig

  -- * Writing IRC clients

  -- | With this library, IRC clients are mostly composed of event
  -- handlers. Event handlers are monadic actions operating in the
  -- 'IRC' monad.

  , IRC
  , send
  , sendBS
  , disconnect
  , reconnect

  -- ** From event handlers

  , module Network.IRC.Client.Events

  -- ** From the outside

  -- | The 'ConnectionConfig', 'InstanceConfig', and some other stuff
  -- are combined in the 'IRCState' type. This can be used to interact
  -- with a client from the outside, by providing a way to run @IRC s
  -- a@ actions.

  , IRCState
  , getIRCState
  , runIRCAction
  , ConnectionState(..)
  , getConnectionState

  -- * Execution
  , runClient

  -- | If an 'IRCState' is constructed with 'newIRCState' and a client
  -- started with 'runClientWith', then 'runIRCAction' can be used to
  -- interact with that client.

  , newIRCState
  , runClientWith

  -- | If the client times out from the server, the 'Timeout'
  -- exception will be thrown, killing it.
  , Timeout(..)

  -- * Concurrency

  -- | A client can manage a collection of threads, which get thrown
  -- the 'Disconnect' exception whenever the client disconnects for
  -- any reason (including a call to 'reconnect'). These can be
  -- created from event handlers to manage long-running tasks.
  , U.fork
  , Disconnect(..)

  -- * Lenses
  , module Network.IRC.Client.Lens

  -- * Utilities
  , module Network.IRC.Client.Utils
  , C.rawMessage
  , C.toByteString
  ) where

import           Control.Concurrent.STM         (newTVarIO)
import           Control.Concurrent.STM.TBMChan (newTBMChanIO)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.ByteString                (ByteString)
import qualified Data.Conduit.Network.TLS       as TLS
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Version                   (showVersion)
import qualified Data.X509                      as X
import qualified Data.X509.CertificateStore     as X
import qualified Data.X509.Validation           as X
import           Network.Connection             as TLS (TLSSettings(..))
import qualified Network.IRC.Conduit            as C
import qualified Network.TLS                    as TLS

import           Network.IRC.Client.Events
import           Network.IRC.Client.Internal
import           Network.IRC.Client.Lens
-- I think exporting 'fork' with 'Disconnect' gives better documentation.
import           Network.IRC.Client.Utils       hiding (fork)
import qualified Network.IRC.Client.Utils       as U

import qualified Paths_irc_client               as Paths


-------------------------------------------------------------------------------
-- Configuration

-- | Connect to a server without TLS.
plainConnection
  :: ByteString
  -- ^ The hostname
  -> Int
  -- ^ The port
  -> ConnectionConfig s
plainConnection host port_ =
  setupInternal (C.ircClient port_ host) defaultOnConnect defaultOnDisconnect noopLogger host port_

-- | How to connect to a server over TLS.
data TLSConfig
  = WithDefaultConfig ByteString Int
  -- ^ Use @<http://hackage.haskell.org/package/irc-conduit/docs/Network-IRC-Conduit.html#t:defaultTLSConfig Network.IRC.Conduit.defaultTLSConfig>@.
  | WithClientConfig TLS.TLSClientConfig
  -- ^ Use the given configuration. The hostname and port are stored
  -- as fields of the 'TLS.TLSClientConfig'.
  | WithVerifier ByteString Int (X.CertificateStore -> TLS.ValidationCache -> X.ServiceID -> X.CertificateChain -> IO [X.FailedReason])
  -- ^ Use @<http://hackage.haskell.org/package/irc-conduit/docs/Network-IRC-Conduit.html#t:defaultTLSConfig Network.IRC.Conduit.defaultTLSConfig>@,
  -- with the given certificate verifier. The certificate verifier is
  -- a function which returns a list of reasons to reject the
  -- certificate.

-- | Connect to a server with TLS.
tlsConnection
  :: TLSConfig
  -- ^ How to initiate the TLS connection
  -> ConnectionConfig s
tlsConnection (WithDefaultConfig host port_) =
    setupInternal (C.ircTLSClient port_ host) defaultOnConnect defaultOnDisconnect noopLogger host port_
tlsConnection (WithClientConfig cfg) =
    setupInternal (C.ircTLSClient' cfg) defaultOnConnect defaultOnDisconnect noopLogger host port_
  where
    host  = TLS.tlsClientHost cfg
    port_ = TLS.tlsClientPort cfg
tlsConnection (WithVerifier host port_ verifier) =
    setupInternal (C.ircTLSClient' cfg) defaultOnConnect defaultOnDisconnect noopLogger host port_
  where
    cfg =
      let cfg0 = C.defaultTLSConfig port_ host
          -- this is a partial pattern match, but because I'm the
          -- author of irc-conduit I can do this.
          TLS.TLSSettings cTLSSettings = TLS.tlsClientTLSSettings cfg0
          cHooks = TLS.clientHooks cTLSSettings
      in cfg0 { TLS.tlsClientTLSSettings = TLS.TLSSettings cTLSSettings
                { TLS.clientHooks = cHooks
                  { TLS.onServerCertificate = verifier }
                }
              }

-- | Construct a default IRC configuration from a nick
defaultInstanceConfig
  :: Text
  -- ^ The nick
  -> InstanceConfig s
defaultInstanceConfig n = InstanceConfig
  { _nick     = n
  , _channels = []
  , _version  = T.append "irc-client-" (T.pack $ showVersion Paths.version)
  , _handlers = defaultEventHandlers
  , _ignore   = []
  }


-------------------------------------------------------------------------------
-- Execution

-- | Connect to the IRC server and run the client: receiving messages
-- and handing them off to handlers as appropriate.
runClient :: MonadIO m
  => ConnectionConfig s
  -> InstanceConfig s
  -> s
  -- ^ The initial value for the user state.
  -> m ()
runClient cconf iconf ustate = newIRCState cconf iconf ustate >>= runClientWith

-- | Like 'runClient', but use the provided initial
-- 'IRCState'.
--
-- Multiple clients should not be run with the same 'IRCState'. The
-- utility of this is to be able to run @IRC s a@ actions in order to
-- interact with the client from the outside.
runClientWith :: MonadIO m => IRCState s -> m ()
runClientWith = runIRCAction runner


-------------------------------------------------------------------------------
-- State

-- | Construct a new IRC state
newIRCState :: MonadIO m
  => ConnectionConfig s
  -> InstanceConfig s
  -> s
  -- ^ The initial value for the user state.
  -> m (IRCState s)
newIRCState cconf iconf ustate = liftIO $ IRCState cconf
  <$> newTVarIO ustate
  <*> newTVarIO iconf
  <*> (newTVarIO =<< newTBMChanIO 16)
  <*> newTVarIO Disconnected
  <*> newTVarIO S.empty
