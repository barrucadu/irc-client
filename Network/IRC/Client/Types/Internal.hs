{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Types.Internal
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes
--
-- Internal types. Most of these are re-exported elsewhere as lenses.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Types.Internal where

import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader   (MonadReader, ReaderT, ask)
import Control.Monad.State    (MonadState(..))
import Data.ByteString        (ByteString)
import Data.Conduit           (Consumer, Producer)
import Data.Conduit.TMChan    (TBMChan)
import Data.Text              (Text)
import Data.Time.Clock        (NominalDiffTime)
import Network.IRC.Conduit    (Event(..), IrcEvent, IrcMessage)


-------------------------------------------------------------------------------
-- The IRC monad

-- | The IRC monad.
newtype IRC s a = IRC { runIRC :: ReaderT (IRCState s) IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IRCState s))

instance MonadState s (IRC s) where
  state f = do
    tvar <- _userState <$> ask
    liftIO . atomically $ do
      (a, s) <- f <$> readTVar tvar
      writeTVar tvar s
      pure a


-------------------------------------------------------------------------------
-- State

-- | The state of an IRC session.
data IRCState s = IRCState { _connectionConfig :: ConnectionConfig s
                           -- ^Read-only connection configuration
                           , _userState        :: TVar s
                           -- ^Mutable user state
                           , _instanceConfig   :: TVar (InstanceConfig s)
                           -- ^Mutable instance configuration in STM
                           , _connectionState  :: TVar ConnectionState
                           -- ^State of the connection.
                           }

-- | The static state of an IRC server connection. Lenses are provided to modify this before
data ConnectionConfig s = ConnectionConfig
  { _func       :: IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()
  -- ^ Function to connect and start the conduits.
  , _sendqueue  :: TBMChan IrcMessage
  -- ^ Message send queue.
  , _server     :: ByteString
  -- ^ The server host.
  , _port       :: Int
  -- ^ The server port.
  , _username   :: Text
  -- ^ Client username; sent to the server during the initial set-up.
  , _realname   :: Text
  -- ^ Client realname; sent to the server during the initial set-up.
  , _password   :: Maybe Text
  -- ^ Client password; sent to the server during the initial set-up.
  , _flood      :: NominalDiffTime
  -- ^ The minimum time between two adjacent messages.
  , _timeout    :: NominalDiffTime
  -- ^ The maximum time between received messages from the server. If no
  -- messages arrive from the server for this period, the client is sent
  -- a 'Timeout' exception and disconnects.
  , _onconnect  :: IRC s ()
  -- ^ Action to run after sending the @PASS@ and @USER@ commands to the
  -- server. The default behaviour is to send the @NICK@ command.
  , _ondisconnect :: IRC s ()
  -- ^ Action to run after disconnecting from the server, both by local
  -- choice and by losing the connection. This is run after tearing down the
  -- connection. The default behaviour is to do nothing.
  , _logfunc    :: Origin -> ByteString -> IO ()
  -- ^ Function to log messages sent to and received from the server.
  }

-- | The updateable state of an IRC connection.
data InstanceConfig s = InstanceConfig
  { _nick     :: Text
  -- ^ Client nick
  , _channels :: [Text]
  -- ^ Current channels: this list both determines the channels to join on
  -- connect, and is modified by the default event handlers when channels
  -- are joined or parted.
  , _version  :: Text
  -- ^ The version is sent in response to the CTCP \"VERSION\" request by
  -- the default event handlers.
  , _handlers :: [EventHandler s]
  -- ^ The registered event handlers
  , _ignore   :: [(Text, Maybe Text)]
  -- ^ List of nicks (optionally restricted to channels) to ignore
  -- messages from. 'Nothing' ignores globally.
  }

-- | The state of the connection.
data ConnectionState = Connected | Disconnecting | Disconnected
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | The origin of a message.
data Origin = FromServer | FromClient
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


-------------------------------------------------------------------------------
-- Events

-- | A function which handles an event.
data EventHandler s = EventHandler
  { _eventPred :: Event Text -> Bool
  -- ^ The predicate to match on.
  , _eventFunc :: Event Text -> IRC s ()
  -- ^ The function to call.
  }

-------------------------------------------------------------------------------
-- * Internal Lens synonyms

-- | See @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Lens.html#t:Lens Control.Lens.Lens.Lens>@.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A @<http://hackage.haskell.org/package/lens/docs/Control-Lens-Type.html#t:Simple Simple>@ 'Lens'.
type Lens' s a = Lens s s a a
