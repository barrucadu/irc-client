{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Network.IRC.Client.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings, ScopedTypeVariables
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

import           Control.Applicative               ((<$>))
import           Control.Concurrent                (forkIO, killThread,
                                                    myThreadId, threadDelay,
                                                    throwTo)
import           Control.Concurrent.STM            (STM, atomically, readTVar,
                                                    readTVarIO, writeTVar)
import           Control.Concurrent.STM.TBMChan    (TBMChan, closeTBMChan,
                                                    isClosedTBMChan,
                                                    isEmptyTBMChan, newTBMChan,
                                                    readTBMChan, writeTBMChan)
import           Control.Monad                     (forM_, unless, void, when)
import           Control.Monad.Catch               (SomeException, catch)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Reader              (ask, runReaderT)
import           Data.ByteString                   (ByteString, isPrefixOf)
import           Data.Conduit                      (ConduitM, await,
                                                    awaitForever, yield, (.|))
import           Data.IORef                        (IORef, newIORef, readIORef,
                                                    writeIORef)
import qualified Data.Set                          as S
import           Data.Text                         (Text)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Time.Clock                   (NominalDiffTime, UTCTime,
                                                    addUTCTime, diffUTCTime,
                                                    getCurrentTime)
import           Data.Time.Format                  (formatTime)
import           Data.Void                         (Void)
import           Network.IRC.Conduit               (Event(..), Message(..),
                                                    Source(..), floodProtector,
                                                    rawMessage, toByteString)

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format                  (defaultTimeLocale)
#else
import           System.Locale                     (defaultTimeLocale)
#endif

import           Network.IRC.Client.Internal.Lens
import           Network.IRC.Client.Internal.Types
import           Network.IRC.Client.Lens


-------------------------------------------------------------------------------
-- * Configuration

-- | Config to connect to a server using the supplied connection
-- function.
setupInternal
  :: (IO () -> ConduitM (Either ByteString (Event ByteString)) Void IO () -> ConduitM () (Message ByteString) IO () -> IO ())
  -- ^ Function to start the network conduits.
  -> IRC s ()
  -- ^ Connect handler
  -> (Maybe SomeException -> IRC s ())
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
runner :: IRC s ()
runner = do
  state <- getIRCState
  let cconf = _connectionConfig state

  -- Set the real- and user-name
  let theUser = get username cconf
  let theReal = get realname cconf
  let thePass = get password cconf

  -- Initialise the IRC session
  let initialise = flip runIRCAction state $ do
        liftIO . atomically $ writeTVar (_connectionState state) Connected
        mapM_ (\p -> sendBS $ rawMessage "PASS" [encodeUtf8 p]) thePass
        sendBS $ rawMessage "USER" [encodeUtf8 theUser, "-", "-", encodeUtf8 theReal]
        _onconnect cconf

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  antiflood <- liftIO $ floodProtector (_flood cconf)

  -- An IORef to keep track of the time of the last received message, to allow a local timeout.
  lastReceived <- liftIO $ newIORef =<< getCurrentTime

  squeue <- liftIO . readTVarIO $ _sendqueue state

  let source = sourceTBMChan squeue
               .| antiflood
               .| logConduit (_logfunc cconf FromClient . toByteString . concealPass)
  let sink   = forgetful
               .| logConduit (_logfunc cconf FromServer . _raw)
               .| eventSink lastReceived state

  -- Fork a thread to disconnect if the timeout elapses.
  mainTId <- liftIO myThreadId
  let time  = _timeout cconf
  let delayms = 1000000 * round time
  let timeoutThread = do
        now <- getCurrentTime
        prior <- readIORef lastReceived
        if diffUTCTime now prior >= time
          then throwTo mainTId Timeout
          else threadDelay delayms >> timeoutThread
  timeoutTId <- liftIO (forkIO timeoutThread)

  -- Start the client.
  (exc :: Maybe SomeException) <- liftIO $ catch
    (_func cconf initialise sink source >> killThread timeoutTId >> pure Nothing)
    (pure . Just)

  disconnect
  _ondisconnect cconf exc

-- | Forget failed decodings.
forgetful :: Monad m => ConduitM (Either a b) b m ()
forgetful = awaitForever go where
  go (Left  _) = return ()
  go (Right b) = yield b

-- | Block on receiving a message and invoke all matching handlers.
eventSink :: MonadIO m => IORef UTCTime -> IRCState s -> ConduitM (Event ByteString) o m ()
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
              (void . flip runIRCAction ircstate . handler (_source event'))
              (matcher event')

    -- If disconnected, do not loop.
    disconnected <- liftIO . atomically $ (==Disconnected) <$> getConnectionState ircstate
    unless disconnected go)

-- | Check if an event is ignored or not.
isIgnored :: MonadIO m => IRCState s -> Event Text -> m Bool
isIgnored ircstate ev = do
  iconf <- liftIO . readTVarIO . _instanceConfig $ ircstate
  let ignoreList = _ignore iconf

  return $
    case _source ev of
      User      n ->  (n, Nothing) `elem` ignoreList
      Channel c n -> ((n, Nothing) `elem` ignoreList) || ((n, Just c) `elem` ignoreList)
      Server  _   -> False

-- |A conduit which logs everything which goes through it.
logConduit :: MonadIO m => (a -> IO ()) -> ConduitM a a m ()
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

-- | Clear passwords from logs.
concealPass :: Message ByteString -> Message ByteString
concealPass (RawMsg msg)
  | "PASS " `isPrefixOf` msg = rawMessage "PASS" ["<password redacted>"]
concealPass m = m


-------------------------------------------------------------------------------
-- * Messaging

-- | Send a message as UTF-8, using TLS if enabled. This blocks if
-- messages are sent too rapidly.
send :: Message Text -> IRC s ()
send = sendBS . fmap encodeUtf8

-- | Send a message, using TLS if enabled. This blocks if messages are
-- sent too rapidly.
sendBS :: Message ByteString -> IRC s ()
sendBS msg = do
  qv <- _sendqueue <$> getIRCState
  liftIO . atomically $ flip writeTBMChan msg =<< readTVar qv


-------------------------------------------------------------------------------
-- * Disconnecting

-- | Disconnect from the server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC s ()
disconnect = do
  s <- getIRCState

  liftIO $ do
    connState <- readTVarIO (_connectionState s)
    case connState of
      Connected -> do
        -- Set the state to @Disconnecting@
        atomically $ writeTVar (_connectionState s) Disconnecting

        -- Wait for all messages to be sent, or a minute has passed.
        timeoutBlock 60 . atomically $ do
          queue <- readTVar (_sendqueue s)
          (||) <$> isEmptyTBMChan queue <*> isClosedTBMChan queue

        -- Close the chan, which closes the sending conduit, and set
        -- the state to @Disconnected@.
        atomically $ do
          closeTBMChan =<< readTVar (_sendqueue s)
          writeTVar (_connectionState s) Disconnected

        -- Kill all managed threads. Don't wait for them to terminate
        -- here, as they might be masking exceptions and not pick up
        -- the 'Disconnect' for a while; just clear the list.
        mapM_ (`throwTo` Disconnect) =<< readTVarIO (_runningThreads s)
        atomically $ writeTVar (_runningThreads s) S.empty

      -- If already disconnected, or disconnecting, do nothing.
      _ -> pure ()

-- | Disconnect from the server (this will wait for all messages to be
-- sent, or a minute to pass), and then connect again.
--
-- This can be called after the client has already disconnected, in
-- which case it will just connect again.
--
-- Like 'runClient' and 'runClientWith', this will not return until
-- the client terminates (ie, disconnects without reconnecting).
reconnect :: IRC s ()
reconnect = do
  disconnect

  -- create a new send queue
  s <- getIRCState
  liftIO . atomically $
    writeTVar (_sendqueue s) =<< newTBMChan 16

  runner


-------------------------------------------------------------------------------
-- * Utils

-- | Interact with a client from the outside, by using its 'IRCState'.
runIRCAction :: MonadIO m => IRC s a -> IRCState s -> m a
runIRCAction ma = liftIO . runReaderT (runIRC ma)

-- | Access the client state.
getIRCState :: IRC s (IRCState s)
getIRCState = ask

-- | Get the connection state from an IRC state.
getConnectionState :: IRCState s -> STM ConnectionState
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

-- | A simple wrapper around a TBMChan. As data is pushed into the
-- channel, the source will read it and pass it down the conduit
-- pipeline. When the channel is closed, the source will close also.
--
-- If the channel fills up, the pipeline will stall until values are
-- read.
--
-- From stm-conduit-3.0.0 (by Clark Gaebel <cg.wowus.cg@gmail.com>)
sourceTBMChan :: MonadIO m => TBMChan a -> ConduitM () a m ()
sourceTBMChan ch = loop where
  loop = do
    a <- liftIO . atomically $ readTBMChan ch
    case a of
      Just x  -> yield x >> loop
      Nothing -> pure ()
