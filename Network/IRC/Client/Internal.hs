{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Network.IRC.Client.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, LambdaCase, OverloadedStrings, RankNTypes, ScopedTypeVariables
--
-- Most of the hairy code. This isn't all internal, due to messy
-- dependencies, but I've tried to make this as \"internal\" as
-- reasonably possible.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Internal
  ( module Network.IRC.Client.Internal
  , module Network.IRC.Client.Internal.Lens
  , module Network.IRC.Client.Internal.Types
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay, throwTo)
import Control.Concurrent.STM (STM, atomically, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask, runReaderT)
import Data.ByteString (ByteString)
import Data.Conduit (Producer, Conduit, Consumer, (=$=), ($=), (=$), await, awaitForever, toProducer, yield)
import Data.Conduit.TMChan (closeTBMChan, isEmptyTBMChan, sourceTBMChan, writeTBMChan)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format (formatTime)
import Network.IRC.Conduit (Event(..), IrcEvent, IrcMessage, Message(..), Source(..), floodProtector, rawMessage, toByteString)

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

import Network.IRC.Client.Internal.Lens
import Network.IRC.Client.Internal.Types
import Network.IRC.Client.Lens


-------------------------------------------------------------------------------
-- * Configuration

-- | Config to connect to a server using the supplied connection
-- function.
setupInternal
  :: (IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ())
  -- ^ Function to start the network conduits.
  -> Irc s ()
  -- ^ Connect handler
  -> (Maybe SomeException -> Irc s ())
  -- ^ Disconnect handler
  -> (Origin -> ByteString -> IO ())
  -- ^ Logging function
  -> ByteString
  -- ^ Server hostname
  -> Int
  -- ^ Server port
  -> ConnectionConfig s
setupInternal f oncon ondis logf host port_ = ConnectionConfig
  { _func         = f
  , _username     = "irc-client"
  , _realname     = "irc-client"
  , _password     = Nothing
  , _server       = host
  , _port         = port_
  , _flood        = 1
  , _timeout      = 300
  , _onconnect    = oncon
  , _ondisconnect = ondis
  , _logfunc      = logf
  }


-------------------------------------------------------------------------------
-- * Event loop

-- | The event loop.
runner :: Irc s ()
runner = do
  state <- getIrcState
  let cconf = _connectionConfig state

  -- Set the real- and user-name
  let theUser = get username cconf
  let theReal = get realname cconf
  let thePass = get password cconf

  -- Initialise the IRC session
  let initialise = runIrcAction state $ do
        liftIO . atomically $ writeTVar (_connectionState state) Connected
        mapM_ (\p -> sendBS $ rawMessage "PASS" [encodeUtf8 p]) thePass
        sendBS $ rawMessage "USER" [encodeUtf8 theUser, "-", "-", encodeUtf8 theReal]
        _onconnect cconf

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  antiflood <- liftIO $ floodProtector (_flood cconf)

  -- An IORef to keep track of the time of the last received message, to allow a local timeout.
  lastReceived <- liftIO $ newIORef =<< getCurrentTime

  let source = toProducer $ sourceTBMChan (_sendqueue state)
                          $= antiflood
                          $= logConduit (_logfunc cconf FromClient . toByteString)
  let sink   = forgetful =$= logConduit (_logfunc cconf FromServer . _raw)
                         =$ eventSink lastReceived state

  -- Fork a thread to disconnect if the timeout elapses.
  mainTId <- liftIO myThreadId
  let time  = _timeout cconf
  let delay = round time
  let timeoutThread = do
        now <- getCurrentTime
        prior <- readIORef lastReceived
        if diffUTCTime now prior >= time
          then throwTo mainTId Timeout
          else threadDelay delay >> timeoutThread
  timeoutTId <- liftIO (forkIO timeoutThread)

  -- Start the client.
  (exc :: Maybe SomeException) <- liftIO $ catch
    (_func cconf initialise sink source >> killThread timeoutTId >> pure Nothing)
    (pure . Just)

  disconnect
  _ondisconnect cconf exc

-- | Forget failed decodings.
forgetful :: Monad m => Conduit (Either a b) m b
forgetful = awaitForever go where
  go (Left  _) = return ()
  go (Right b) = yield b

-- | Block on receiving a message and invoke all matching handlers
-- concurrently.
eventSink :: MonadIO m => IORef UTCTime -> IrcState s -> Consumer IrcEvent m ()
eventSink lastReceived ircstate = go where
  go = await >>= maybe (return ()) (\event -> do
    -- Record the current time.
    now <- liftIO getCurrentTime
    liftIO $ writeIORef lastReceived now

    -- Handle the event.
    let event' = decodeUtf8 <$> event
    ignored <- isIgnored ircstate event'
    unless ignored . liftIO $ do
      iconf <- snapshot instanceConfig ircstate
      forM_ (get handlers iconf) $ \(EventHandler matcher handler) ->
        maybe (pure ())
              (void . forkIO . runIrcAction ircstate . handler (_source event'))
              (matcher event')

    -- If disconnected, do not loop.
    disconnected <- liftIO . atomically $ (==Disconnected) <$> getConnectionState ircstate
    unless disconnected go)

-- | Check if an event is ignored or not.
isIgnored :: MonadIO m => IrcState s -> Event Text -> m Bool
isIgnored ircstate ev = do
  iconf <- liftIO . atomically . readTVar . _instanceConfig $ ircstate
  let ignoreList = _ignore iconf

  return $
    case _source ev of
      User      n ->  (n, Nothing) `elem` ignoreList
      Channel c n -> ((n, Nothing) `elem` ignoreList) || ((n, Just c) `elem` ignoreList)
      Server  _   -> False

-- |A conduit which logs everything which goes through it.
logConduit :: MonadIO m => (a -> IO ()) -> Conduit a m a
logConduit logf = awaitForever $ \x -> do
  -- Call the logging function
  liftIO $ logf x

  -- And pass the message on
  yield x

-- | Print messages to stdout, with the current time.
stdoutLogger :: Origin -> ByteString -> IO ()
stdoutLogger origin x = do
  now <- getCurrentTime

  putStrLn $ unwords
    [ formatTime defaultTimeLocale "%c" now
    , if origin == FromServer then "<---" else "--->"
    , init . tail $ show x
    ]

-- | Append messages to a file, with the current time.
fileLogger :: FilePath -> Origin -> ByteString -> IO ()
fileLogger fp origin x = do
  now <- getCurrentTime

  appendFile fp $ unwords
    [ formatTime defaultTimeLocale "%c" now
    , if origin == FromServer then "--->" else "<---"
    , init . tail $ show x
    , "\n"
    ]

-- | Do no logging.
noopLogger :: a -> b -> IO ()
noopLogger _ _ = return ()


-------------------------------------------------------------------------------
-- * Messaging

-- | Send a message as UTF-8, using TLS if enabled. This blocks if
-- messages are sent too rapidly.
send :: Message Text -> Irc s ()
send = sendBS . fmap encodeUtf8

-- | Send a message, using TLS if enabled. This blocks if messages are
-- sent too rapidly.
sendBS :: IrcMessage -> Irc s ()
sendBS msg = do
  queue <- _sendqueue <$> getIrcState
  liftIO . atomically $ writeTBMChan queue msg


-------------------------------------------------------------------------------
-- * Disconnecting

-- | Disconnect from the server, properly tearing down the TLS session
-- (if there is one).
disconnect :: Irc s ()
disconnect = do
  s <- getIrcState

  connState <- liftIO . atomically . readTVar $ _connectionState s
  case connState of
    Connected -> do
      -- Set the state to @Disconnecting@
      liftIO . atomically $ writeTVar (_connectionState s) Disconnecting

      -- Wait for all messages to be sent, or a minute has passed.
      timeoutBlock 60 . atomically $ isEmptyTBMChan (_sendqueue s)

      -- Then close the connection
      disconnectNow

    -- If already disconnected, or disconnecting, do nothing.
    _ -> pure ()

-- | Disconnect immediately, without waiting for messages to be sent.
disconnectNow :: Irc s ()
disconnectNow = do
  s <- getIrcState
  liftIO . atomically $ do
    closeTBMChan (_sendqueue s)
    writeTVar (_connectionState s) Disconnected


-------------------------------------------------------------------------------
-- * Utils

-- | Interact with a client from the outside, by using its 'IrcState'.
runIrcAction :: MonadIO m => IrcState s -> Irc s a -> m a
runIrcAction s = liftIO . flip runReaderT s . runIrc

-- | Access the client state.
getIrcState :: Irc s (IrcState s)
getIrcState = ask

-- | Get the connection state from an IRC state.
getConnectionState :: IrcState s -> STM ConnectionState
getConnectionState = readTVar . _connectionState

-- | Block until an action is successful or a timeout is reached.
timeoutBlock :: MonadIO m => NominalDiffTime -> IO Bool -> m ()
timeoutBlock dt check = liftIO $ do
  finish <- addUTCTime dt <$> getCurrentTime
  let wait = do
        now  <- getCurrentTime
        cond <- check
        when (now < finish && not cond) wait
  wait
