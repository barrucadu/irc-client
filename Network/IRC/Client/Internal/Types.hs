{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Network.IRC.Client.Internal.Types
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses
--
-- Internal types. Most of these are re-exported elsewhere as lenses.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Internal.Types where

import           Control.Applicative            (Alternative)
import           Control.Concurrent             (ThreadId)
import           Control.Concurrent.STM         (TVar, atomically, readTVar,
                                                 readTVarIO, writeTVar)
import           Control.Concurrent.STM.TBMChan (TBMChan)
import           Control.Monad                  (MonadPlus)
import           Control.Monad.Catch            (Exception, MonadCatch,
                                                 MonadMask, MonadThrow,
                                                 SomeException)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Control.Monad.Reader           (MonadReader, ReaderT, asks)
import           Control.Monad.State            (MonadState(..))
import           Data.ByteString                (ByteString)
import           Data.Conduit                   (ConduitM)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import           Data.Time.Clock                (NominalDiffTime)
import           Data.Void                      (Void)
import           Network.IRC.Conduit            (Event(..), Message, Source)


-------------------------------------------------------------------------------
-- * The IRC monad

-- | The IRC monad.
newtype IRC s a = IRC { runIRC :: ReaderT (IRCState s) IO a }
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus, MonadIO, MonadReader (IRCState s), MonadThrow, MonadCatch, MonadMask)

instance MonadState s (IRC s) where
  state f = do
    tvar <- asks _userState
    liftIO . atomically $ do
      (a, s) <- f <$> readTVar tvar
      writeTVar tvar s
      pure a
  get = do
    tvar <- asks _userState
    liftIO $ readTVarIO tvar
  put s = do
    tvar <- asks _userState
    liftIO $ atomically (writeTVar tvar s)

-------------------------------------------------------------------------------
-- * State

-- | The state of an IRC session.
data IRCState s = IRCState
  { _connectionConfig :: ConnectionConfig s
  -- ^ Read-only connection configuration
  , _userState        :: TVar s
  -- ^ Mutable user state
  , _instanceConfig   :: TVar (InstanceConfig s)
  -- ^ Mutable instance configuration in STM
  , _sendqueue        :: TVar (TBMChan (Message ByteString))
  -- ^ Message send queue.
  , _connectionState  :: TVar ConnectionState
  -- ^ State of the connection.
  , _runningThreads   :: TVar (S.Set ThreadId)
  -- ^ Threads which will be killed when the client disconnects.
  }

-- | The static state of an IRC server connection.
data ConnectionConfig s = ConnectionConfig
  { _func       :: IO () -> ConduitM (Either ByteString (Event ByteString)) Void IO () -> ConduitM () (Message ByteString) IO () -> IO ()
  -- ^ Function to connect and start the conduits.
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
  -- ^ The maximum time (in seconds) between received messages from
  -- the server. If no messages arrive from the server for this
  -- period, the client is sent a 'Timeout' exception and disconnects.
  , _onconnect  :: IRC s ()
  -- ^ Action to run after sending the @PASS@ and @USER@ commands to the
  -- server. The default behaviour is to send the @NICK@ command.
  , _ondisconnect :: Maybe SomeException -> IRC s ()
  -- ^ Action to run after disconnecting from the server, both by
  -- local choice and by losing the connection. This is run after
  -- tearing down the connection. If the connection terminated due to
  -- an exception, it is given. The default behaviour is to reconnect
  -- if a timeout, otherwise rethrow any exception.
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
  -- ^ The registered event handlers. The order in this list is the
  -- order in which they are executed.
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
-- * Events

-- | A function which handles an event.
data EventHandler s where
  EventHandler
    :: (Event Text -> Maybe b)
    -> (Source Text -> b -> IRC s ())
    -> EventHandler s


-------------------------------------------------------------------------------
-- * Exceptions

-- | Exception thrown to kill the client if the timeout elapses with
-- nothing received from the server.
data Timeout = Timeout
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception Timeout

-- | Exception thrown to all managed threads when the client
-- disconnects.
data Disconnect = Disconnect
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Exception Disconnect
