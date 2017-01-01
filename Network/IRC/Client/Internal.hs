{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Network.IRC.Client.Internal
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings, RankNTypes, ScopedTypeVariables
--
-- Most of the hairy code. This isn't all internal, due to messy
-- dependencies, but I've tried to make this as \"internal\" as
-- reasonably possible.
--
-- This module is NOT considered to form part of the public interface
-- of this library.
module Network.IRC.Client.Internal where

import Control.Applicative        ((<$>))
import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (atomically, readTVar, writeTVar)
import Control.Exception          (SomeException, catch, throwIO)
import Control.Monad              (unless, when)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString            (ByteString)
import Data.Conduit               (Producer, Conduit, Consumer, (=$=), ($=), (=$), await, awaitForever, toProducer, yield)
import Data.Conduit.TMChan        (closeTBMChan, isEmptyTBMChan, newTBMChanIO, sourceTBMChan, writeTBMChan)
import Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import Data.Time.Clock            (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Format           (formatTime)
import Network.IRC.Conduit        (IrcEvent, IrcMessage, floodProtector, rawMessage, toByteString)

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale    (defaultTimeLocale)
#endif

import Network.IRC.Client.Types
import Network.IRC.Client.Types.Internal
import Network.IRC.Client.Utils.Lens

-- * Connecting to an IRC network

-- | Connect to a server using the supplied connection function.
connectInternal :: MonadIO m
  => (IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ())
  -- ^ Function to start the network conduits.
  -> StatefulIRC s ()
  -- ^ Connect handler
  -> StatefulIRC s ()
  -- ^ Disconnect handler
  -> (Origin -> ByteString -> IO ())
  -- ^ Logging function
  -> ByteString
  -- ^ Server hostname
  -> Int
  -- ^ Server port
  -> NominalDiffTime
  -- ^ Flood timeout
  -> m (ConnectionConfig s)
connectInternal f oncon ondis logf host port flood = liftIO $ do
  queueS <- newTBMChanIO 16

  return ConnectionConfig
    { _func         = f
    , _sendqueue    = queueS
    , _server       = host
    , _port         = port
    , _flood        = flood
    , _onconnect    = oncon
    , _ondisconnect = ondis
    , _logfunc      = logf
    }

-- * Event loop

-- | The event loop.
runner :: StatefulIRC s ()
runner = do
  state <- getIrcState
  iconf <- snapshot instanceConfig state
  let cconf = _connectionConfig state

  -- Set the real- and user-name
  let theUser = get username iconf
  let theReal = get realname iconf
  let thePass = get password iconf

  -- Initialise the IRC session
  let initialise = flip runReaderT state $ do
        liftIO . atomically $ writeTVar (_connectionState state) Connected
        mapM_ (\p -> sendBS $ rawMessage "PASS" [encodeUtf8 p]) thePass
        sendBS $ rawMessage "USER" [encodeUtf8 theUser, "-", "-", encodeUtf8 theReal]
        _onconnect cconf

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  antiflood <- liftIO $ floodProtector (_flood cconf)

  let source = toProducer $ sourceTBMChan (_sendqueue cconf)
                          $= antiflood
                          $= logConduit (_logfunc cconf FromClient . toByteString)
  let sink   = forgetful =$= logConduit (_logfunc cconf FromServer . _raw)
                         =$ eventSink state

  (exc :: Maybe SomeException) <- liftIO $ catch
    (_func cconf initialise sink source >> pure Nothing)
    (pure . Just)

  disconnect
  _ondisconnect cconf

  -- If the connection terminated due to an exception, rethrow it.
  liftIO $ maybe (pure ()) throwIO exc

-- | Forget failed decodings.
forgetful :: Monad m => Conduit (Either a b) m b
forgetful = awaitForever go where
  go (Left  _) = return ()
  go (Right b) = yield b

-- | Block on receiving a message and invoke all matching handlers
-- concurrently.
eventSink :: MonadIO m => IRCState s -> Consumer IrcEvent m ()
eventSink ircstate = go where
  go = await >>= maybe (return ()) (\event -> do
    let event'  = decodeUtf8 <$> event
    ignored <- isIgnored ircstate event'
    unless ignored $ do
      hs <- getHandlersFor event' . get handlers <$> snapshot instanceConfig ircstate
      liftIO $ mapM_ (\h -> forkIO $ runReaderT (h event') ircstate) hs

    -- If disconnected, do not loop.
    disconnected <- liftIO . atomically $ (==Disconnected) <$> getConnectionState ircstate
    unless disconnected go)

-- | Check if an event is ignored or not.
isIgnored :: MonadIO m => IRCState s -> UnicodeEvent -> m Bool
isIgnored ircstate ev = do
  iconf <- liftIO . atomically . readTVar . _instanceConfig $ ircstate
  let ignoreList = _ignore iconf

  return $
    case _source ev of
      User      n ->  (n, Nothing) `elem` ignoreList
      Channel c n -> ((n, Nothing) `elem` ignoreList) || ((n, Just c) `elem` ignoreList)
      Server  _   -> False

-- |Get the event handlers for an event.
getHandlersFor :: Event a -> [EventHandler s] -> [UnicodeEvent -> StatefulIRC s ()]
getHandlersFor e ehs = [_eventFunc eh | eh <- ehs, _matchType eh `elem` [EEverything, eventType e]]

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

-- * Messaging

-- | Send a message as UTF-8, using TLS if enabled. This blocks if
-- messages are sent too rapidly.
send :: UnicodeMessage -> StatefulIRC s ()
send = sendBS . fmap encodeUtf8

-- | Send a message, using TLS if enabled. This blocks if messages are
-- sent too rapidly.
sendBS :: IrcMessage -> StatefulIRC s ()
sendBS msg = do
  queue <- _sendqueue . _connectionConfig <$> getIrcState
  liftIO . atomically $ writeTBMChan queue msg

-- * Disconnecting

-- | Disconnect from the server, properly tearing down the TLS session
-- (if there is one).
disconnect :: StatefulIRC s ()
disconnect = do
  s <- getIrcState

  connState <- liftIO . atomically . readTVar $ _connectionState s
  case connState of
    Connected -> do
      -- Set the state to @Disconnecting@
      liftIO . atomically $ writeTVar (_connectionState s) Disconnecting

      -- Wait for all messages to be sent, or a minute has passed.
      let queueS = _sendqueue (_connectionConfig s)
      timeout 60 . atomically $ isEmptyTBMChan queueS

      -- Then close the connection
      disconnectNow

    -- If already disconnected, or disconnecting, do nothing.
    _ -> pure ()

-- | Disconnect immediately, without waiting for messages to be sent.
disconnectNow :: StatefulIRC s ()
disconnectNow = do
  s <- getIrcState
  liftIO . atomically $ do
    let queueS = _sendqueue (_connectionConfig s)
    closeTBMChan queueS
    writeTVar (_connectionState s) Disconnected

-- | Block until an action is successful or a timeout is reached.
timeout :: MonadIO m => NominalDiffTime -> IO Bool -> m ()
timeout dt check = liftIO $ do
  finish <- addUTCTime dt <$> getCurrentTime
  let wait = do
        now  <- getCurrentTime
        cond <- check
        when (now < finish && not cond) wait
  wait
