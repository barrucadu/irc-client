{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |Most of the hairy code. This isn't all internal, due to messy
-- dependencies, but I've tried to make this as "internal" as
-- reasonably possible.
module Network.IRC.Client.Internal where

import Control.Applicative        ((<$>))
import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (atomically, retry)
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
import System.Locale              (defaultTimeLocale)

-- *Connecting to an IRC network

-- |Connect to a server
connect' :: MonadIO m
         => (Int -> ByteString -> IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ())
         -> IRC ()
         -> ByteString
         -> Int
         -> NominalDiffTime
         -> m ConnectionConfig
connect' f dcHandler host port flood = liftIO $ do
  queueS <- newTBMChanIO 16

  return ConnectionConfig
             { _func       = f
             , _sendqueue  = queueS
             , _server     = host
             , _port       = port
             , _flood      = flood
             , _disconnect = dcHandler
             }

-- *Event loop

-- |The event loop.
runner :: IRC ()
runner = do
  state <- ircState

  -- Set the nick and username
  theNick <- _nick     <$> instanceConfig
  theUser <- _username <$> instanceConfig
  theReal <- _realname <$> instanceConfig

  let initialise = flip runReaderT state $ do
        sendBS $ rawMessage "USER" [encodeUtf8 theUser, "-", "-", encodeUtf8 theReal]
        send $ Nick theNick
        mapM_ (send . Join) . _channels <$> instanceConfig
        return ()

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  flood  <- _flood     <$> connectionConfig
  func   <- _func      <$> connectionConfig
  port   <- _port      <$> connectionConfig
  server <- _server    <$> connectionConfig
  queue  <- _sendqueue <$> connectionConfig

  antiflood <- liftIO $ floodProtector flood

  dchandler <- _disconnect <$> connectionConfig

  let source = toProducer $ sourceTBMChan queue $= antiflood $= logConduit False toByteString
  let sink   = forgetful =$= logConduit True _raw =$ eventSink state

  liftIO $ func port server initialise sink source

  disconnect
  dchandler

-- |Forget failed decodings
forgetful :: Monad m => Conduit (Either a b) m b
forgetful = awaitForever go
  where
    go (Left  _) = return ()
    go (Right b) = yield b

-- |Block on receiving a message and invoke all matching handlers
-- simultaneously.
eventSink :: MonadIO m => IRCState -> Consumer IrcEvent m ()
eventSink ircstate = awaitForever $ \event -> do
  let event' = decodeUtf8 <$> event

  handlers <- getHandlersFor event' . _eventHandlers <$> getInstanceConfig' ircstate
  liftIO $ mapM_ (\h -> forkIO $ runReaderT (h event') ircstate) handlers

-- |Get the event handlers for an event.
getHandlersFor :: Event a -> [EventHandler] -> [UnicodeEvent -> IRC ()]
getHandlersFor e ehs = [_eventFunc eh | eh <- ehs, _matchType eh `elem` [EEverything, eventType e]]

-- |A conduit which logs everything which goes through it.
logConduit :: MonadIO m => Bool -> (a -> ByteString) -> Conduit a m a
logConduit fromsrv f = awaitForever $ \x -> do
  -- Print the log
  liftIO $ do
    now <- getCurrentTime

    putStrLn $ unwords [ formatTime defaultTimeLocale "%c" now
                       , if fromsrv then "<---" else "--->"
                       , init . tail . show $ f x
                       ]

  -- And pass the message on
  yield x

-- *Messaging

-- |Send a message as UTF-8, using TLS if enabled. This blocks if
-- messages are sent too rapidly.
send :: UnicodeMessage -> IRC ()
send = sendBS . fmap encodeUtf8

-- |Send a message, using TLS if enabled. This blocks if messages are
-- sent too rapidly.
sendBS :: IrcMessage -> IRC ()
sendBS msg = do
  queue <- _sendqueue <$> connectionConfig
  liftIO . atomically $ writeTBMChan queue msg

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC ()
disconnect = do
  queueS <- _sendqueue <$> connectionConfig

  -- Wait for all messages to be sent
  liftIO . atomically $ do
    empty <- isEmptyTBMChan queueS
    unless empty retry

  -- Then close the connection
  disconnectNow

-- |Disconnect immediately, without waiting for messages to be sent
disconnectNow :: IRC ()
disconnectNow = do
  queueS <- _sendqueue <$> connectionConfig
  liftIO . atomically $ closeTBMChan queueS
