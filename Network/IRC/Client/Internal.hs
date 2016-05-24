{-# LANGUAGE CPP                #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Most of the hairy code. This isn't all internal, due to messy
-- dependencies, but I've tried to make this as \"internal\" as
-- reasonably possible.
module Network.IRC.Client.Internal where

import Control.Applicative        ((<$>))
import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (atomically, readTVar, retry)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString            (ByteString)
import Data.Conduit               (Producer, Conduit, Consumer, (=$=), ($=), (=$), awaitForever, toProducer, yield)
import Data.Conduit.TMChan        (closeTBMChan, isEmptyTBMChan, newTBMChanIO, sourceTBMChan, writeTBMChan)
import Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import Data.Time.Clock            (NominalDiffTime, getCurrentTime)
import Data.Time.Format           (formatTime)
import Network.IRC.Conduit        (IrcEvent, IrcMessage, floodProtector, rawMessage, toByteString)
import Network.IRC.Client.Types

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale    (defaultTimeLocale)
#endif

-- * Connecting to an IRC network

-- | Connect to a server using the supplied connection function.
connectInternal :: MonadIO m
  => (Int -> ByteString -> IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ())
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
  -- ^ Server post
  -> NominalDiffTime
  -- ^ Flood timeout
  -> m (ConnectionConfig s)
connectInternal f onconnect ondisconnect logf host port flood = liftIO $ do
  queueS <- newTBMChanIO 16

  return ConnectionConfig
    { _func         = f
    , _sendqueue    = queueS
    , _server       = host
    , _port         = port
    , _flood        = flood
    , _onconnect    = onconnect
    , _ondisconnect = ondisconnect
    , _logfunc      = logf
    }

-- * Event loop

-- | The event loop.
runner :: StatefulIRC s ()
runner = do
  state <- ircState

  -- Set the real- and user-name
  theUser  <- _username <$> instanceConfig
  theReal  <- _realname <$> instanceConfig
  password <- _password <$> instanceConfig

  -- Initialise the IRC session
  let initialise = flip runReaderT state $ do
        mapM_ (\p -> sendBS $ rawMessage "PASS" [encodeUtf8 p]) password
        sendBS $ rawMessage "USER" [encodeUtf8 theUser, "-", "-", encodeUtf8 theReal]
        _onconnect =<< connectionConfig

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  flood  <- _flood     <$> connectionConfig
  func   <- _func      <$> connectionConfig
  logf   <- _logfunc   <$> connectionConfig
  port   <- _port      <$> connectionConfig
  queue  <- _sendqueue <$> connectionConfig
  server <- _server    <$> connectionConfig

  antiflood <- liftIO $ floodProtector flood

  dchandler <- _ondisconnect <$> connectionConfig

  let source = toProducer $ sourceTBMChan queue $= antiflood $= logConduit (logf FromClient . toByteString)
  let sink   = forgetful =$= logConduit (logf FromServer . _raw) =$ eventSink state

  liftIO $ func port server initialise sink source

  disconnect
  dchandler

-- | Forget failed decodings.
forgetful :: Monad m => Conduit (Either a b) m b
forgetful = awaitForever go where
  go (Left  _) = return ()
  go (Right b) = yield b

-- | Block on receiving a message and invoke all matching handlers
-- concurrently.
eventSink :: MonadIO m => IRCState s -> Consumer IrcEvent m ()
eventSink ircstate = awaitForever $ \event -> do
  let event'  = decodeUtf8 <$> event
  ignored <- isIgnored ircstate event'
  unless ignored $ do
    handlers <- getHandlersFor event' . _eventHandlers <$> getInstanceConfig' ircstate
    liftIO $ mapM_ (\h -> forkIO $ runReaderT (h event') ircstate) handlers

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
  queue <- _sendqueue <$> connectionConfig
  liftIO . atomically $ writeTBMChan queue msg

-- * Disconnecting

-- | Disconnect from the server, properly tearing down the TLS session
-- (if there is one).
disconnect :: StatefulIRC s ()
disconnect = do
  queueS <- _sendqueue <$> connectionConfig

  -- Wait for all messages to be sent
  liftIO . atomically $ do
    empty <- isEmptyTBMChan queueS
    unless empty retry

  -- Then close the connection
  disconnectNow

-- | Disconnect immediately, without waiting for messages to be sent.
disconnectNow :: StatefulIRC s ()
disconnectNow = do
  queueS <- _sendqueue <$> connectionConfig
  liftIO . atomically $ closeTBMChan queueS
