{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

-- |Entry point to the Integrated Data Thought Entity.
module Network.IRC.IDTE
    ( module Network.IRC.IDTE.Types
    , connect
    , connectWithTLS
    , start
    , start'
    , send
    , sendBS
    , disconnect
    , defaultIRCConf
    , defaultDisconnectHandler
    , setNick
    , leaveChannel
    , reply
    , ctcp
    , ctcpReply
    ) where

import Control.Applicative        ((<$>))
import Control.Arrow              (first)
import Control.Concurrent         (forkIO)
import Control.Concurrent.STM     (STM, TVar, atomically, readTVar, retry, writeTVar)
import Control.Monad              (unless)
import Control.Monad.IO.Class     (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString            (ByteString)
import Data.Char                  (isAlphaNum)
import Data.Conduit               (Producer, Conduit, Consumer, ($=), (=$), awaitForever, toProducer, yield)
import Data.Conduit.TMChan        (closeTBMChan, isEmptyTBMChan, newTBMChanIO, sourceTBMChan, writeTBMChan)
import Data.Maybe                 (fromMaybe)
import Data.Monoid                ((<>))
import Data.Text                  (Text, breakOn, takeEnd, toUpper)
import Data.Text.Encoding         (decodeUtf8, encodeUtf8)
import Data.Time.Clock            (NominalDiffTime, getCurrentTime)
import Data.Time.Format           (formatTime)
import Network.IRC.CTCP           (fromCTCP, toCTCP)
import Network.IRC.Conduit        (IrcEvent, IrcMessage, floodProtector, ircClient, ircTLSClient, rawMessage, toByteString)
import Network.IRC.IDTE.Types
import System.Locale              (defaultTimeLocale)

import qualified Data.Text as T

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => ByteString -> Int -> NominalDiffTime -> m ConnectionConfig
connect = connect' ircClient

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => ByteString -> Int -> NominalDiffTime -> m ConnectionConfig
connectWithTLS = connect' ircTLSClient

-- |Connect to a server
connect' :: MonadIO m => (Int -> ByteString -> IO () -> Consumer IrcEvent IO () -> Producer IO IrcMessage -> IO ()) -> ByteString -> Int -> NominalDiffTime -> m ConnectionConfig
connect' f host port flood = liftIO $ do
  queueS <- newTBMChanIO 16

  return ConnectionConfig
             { _func       = f
             , _sendqueue  = queueS
             , _server     = host
             , _port       = port
             , _flood      = flood
             , _disconnect = defaultDisconnectHandler
             }

-- *Event loop

-- |Run the event loop for a server, receiving messages and handing
-- them off to handlers as appropriate.
start :: MonadIO m => ConnectionConfig -> InstanceConfig -> m ()
start cconf iconf = newIRCState cconf iconf >>= start'

-- |Like 'start', but use the provided initial state.
start' :: MonadIO m => IRCState -> m ()
start' = liftIO . runReaderT runner

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
  let sink   = logConduit True _raw =$ eventSink state

  liftIO $ func port server initialise sink source

  disconnect
  dchandler

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

-- |The default disconnect handler: do nothing.
defaultDisconnectHandler :: IRC ()
defaultDisconnectHandler = return ()

-- *Default configuration

-- |Construct a default IRC configuration from a nick
defaultIRCConf :: Text -> InstanceConfig
defaultIRCConf n = InstanceConfig
                     { _nick          = n
                     , _username      = n
                     , _realname      = n
                     , _channels      = []
                     , _ctcpVer       = "idte-0.0.0.1"
                     , _eventHandlers = [ EventHandler "Respond to server PING requests"  EPing pingHandler
                                        , EventHandler "Respond to CTCP PING requests"    ECTCP ctcpPingHandler
                                        , EventHandler "Respond to CTCP VERSION requests" ECTCP ctcpVersionHandler
                                        , EventHandler "Respond to CTCP TIME requests"    ECTCP ctcpTimeHandler
                                        , EventHandler "Update the nick upon welcome"     ENumeric welcomeNick
                                        , EventHandler "Mangle the nick on collision"     ENumeric nickMangler
                                        , EventHandler "Update the channel list on JOIN"  ENumeric joinHandler
                                        , EventHandler "Update the channel lift on KICK"  EKick    kickHandler
                                        ]
                     }

-- |Respond to pings
pingHandler :: UnicodeEvent -> IRC ()
pingHandler ev =
  case _message ev of
    Ping s1 Nothing  -> send $ Pong s1
    Ping _ (Just s2) -> send $ Pong s2
    _ -> return ()

-- |Respond to CTCP PINGs
ctcpPingHandler :: UnicodeEvent -> IRC ()
ctcpPingHandler ev =
  case (_source ev, _message ev) of
    (User n, Privmsg _ (Left ctcpbs)) ->
      case first toUpper $ fromCTCP ctcpbs of
        ("PING", xs) -> send $ ctcpReply n "PING" xs
        _ -> return ()
    _ -> return ()

-- |Respond to CTCP VERSIONs
ctcpVersionHandler :: UnicodeEvent -> IRC ()
ctcpVersionHandler ev = do
  ver <- _ctcpVer <$> instanceConfig
  case (_source ev, _message ev) of
    (User n, Privmsg _ (Left ctcpbs)) ->
      case first toUpper $ fromCTCP ctcpbs of
        ("VERSION", _) -> send $ ctcpReply n "VERSION" [ver]
        _ -> return ()
    _ -> return ()

-- |Respond to CTCP TIMEs
ctcpTimeHandler :: UnicodeEvent -> IRC ()
ctcpTimeHandler ev = do
  now <- liftIO getCurrentTime
  case (_source ev, _message ev) of
    (User n, Privmsg _ (Left ctcpbs)) ->
      case first toUpper $ fromCTCP ctcpbs of
        ("TIME", _) -> send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
        _ -> return ()
    _ -> return ()

-- |Update the nick upon welcome, as it may not be what we requested
-- (eg, in the case of a nick too long).
welcomeNick :: UnicodeEvent -> IRC ()
welcomeNick ev = case _message ev of
                   Numeric 001 (srvNick:_) -> do
                     tvarI <- instanceConfigTVar

                     liftIO . atomically $ do
                       iconf <- readTVar tvarI
                       writeTVar tvarI iconf { _nick = srvNick }
                   _ -> return ()

-- |Mangle the nick if there's a collision when we set it
nickMangler :: UnicodeEvent -> IRC ()
nickMangler ev = do
  theNick <- _nick <$> instanceConfig

  -- Produce the new nick based on what the server thinks our nick is,
  -- not what we think.
  let Numeric _ (_:srvNick:_) = _message ev

  -- If the length of our nick and the server's idea of our nick
  -- differ, it was truncated - so calculate the allowable length.
  let nicklen = if T.length srvNick /= T.length theNick
                then Just $ T.length srvNick
                else Nothing

  case _message ev of
    -- ERR_ERRONEUSNICKNAME: Bad characters in nick
    Numeric 432 _ -> setNick . trunc nicklen $ fresh srvNick
    -- ERR_NICKNAMEINUSE: Nick in use
    Numeric 433 _ -> setNick . trunc nicklen $ mangle srvNick
    -- ERR_NICKCOLLISION: Nick registered
    Numeric 436 _ -> setNick . trunc nicklen $ mangle srvNick
    _ -> return ()

  where fresh n  = let n' = T.filter isAlphaNum n
                   in if T.length n' == 0
                      then "f"
                      else n'

        mangle n = (n <> "1") `fromMaybe` charsubst n

        -- Truncate a nick, if there is a known length limit.
        trunc (Just len) txt = takeEnd len txt
        trunc Nothing    txt = txt

        -- List of substring substitutions. It's important that
        -- these don't contain any loops!
        charsubst = transform [ ("i", "1")
                              , ("I", "1")
                              , ("l", "1")
                              , ("L", "1")
                              , ("o", "0")
                              , ("O", "0")
                              , ("A", "4")
                              , ("0", "1")
                              , ("1", "2")
                              , ("2", "3")
                              , ("3", "4")
                              , ("4", "5")
                              , ("5", "6")
                              , ("6", "7")
                              , ("7", "8")
                              , ("8", "9")
                              , ("9", "-")
                              ]

        -- Attempt to transform some text by the substitutions.
        transform ((from, to):trs) txt = case breakOn' from txt of
                                           Just (before, after) -> Just $ before <> to <> after
                                           Nothing -> transform trs txt
        transform [] _ = Nothing

        breakOn' delim txt = let (before, after) = breakOn delim txt
                             in if T.length after >= T.length delim
                                then Just (before, T.drop (T.length delim) after)
                                else Nothing

-- |Upon receiving a RPL_TOPIC, add the channel to the list (if not
-- already present).
joinHandler :: UnicodeEvent -> IRC ()
joinHandler ev = case _message ev of
                   Numeric 332 (c:_) -> do
                     tvarI <- instanceConfigTVar

                     liftIO . atomically $ do
                       iconf <- readTVar tvarI
                       unless (c `elem` _channels iconf) $
                         writeTVar tvarI iconf { _channels = c : _channels iconf }

                   _ -> return ()

-- |Update the channel list upon being kicked.
kickHandler :: UnicodeEvent -> IRC ()
kickHandler ev = do
  theNick <- _nick <$> instanceConfig
  tvarI   <- instanceConfigTVar

  case (_source ev, _message ev) of
    (Channel c _, Kick n _ _) | n == theNick -> liftIO . atomically $ delChan tvarI c
                              | otherwise   -> return ()
    _ -> return ()

-- *Utilities

-- |Update the nick in the instance configuration and also send an
-- update message to the server.
setNick :: Text -> IRC ()
setNick new = do
  tvarI <- instanceConfigTVar

  liftIO . atomically $ do
    iconf <- readTVar tvarI
    writeTVar tvarI iconf { _nick = new }

  send $ Nick new

-- |Update the channel list in the instance configuration and also
-- part the channel.
leaveChannel :: Text -> Maybe Text -> IRC ()
leaveChannel chan reason = do
  tvarI <- instanceConfigTVar
  liftIO . atomically $ delChan tvarI chan

  send $ Part chan reason

-- |Remove a channel from the list.
delChan :: TVar InstanceConfig -> Text -> STM ()
delChan tvarI chan = do
  iconf <- readTVar tvarI
  writeTVar tvarI iconf { _channels = filter (/=chan) $ _channels iconf }

-- |Send a message to the source of an event.
reply :: UnicodeEvent -> Text -> IRC ()
reply ev txt = case _source ev of
                 Channel c _ -> send $ Privmsg c $ Right txt
                 User n      -> send $ Privmsg n $ Right txt
                 _           -> return ()

-- |Construct a privmsg containing a CTCP
ctcp :: Text -> Text -> [Text] -> UnicodeMessage
ctcp t command args = Privmsg t . Left $ toCTCP command args

-- |Construct a notice containing a CTCP
ctcpReply :: Text -> Text -> [Text] -> UnicodeMessage
ctcpReply t command args = Notice t . Left $ toCTCP command args
