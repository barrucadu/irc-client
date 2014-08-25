{-# LANGUAGE OverloadedStrings #-}

-- |Entry point to the Integrated Data Thought Entity.
module Network.IRC.IDTE
    ( module Network.IRC.IDTE.Types
    , module Network.IRC.IDTE.Messages
    , connect
    , connectWithTLS
    , connectWithTLS'
    , start
    , start'
    , send
    , disconnect
    , defaultIRCConf
    , defaultDisconnectHandler
    , setNick
    , leaveChannel
    , reply
    ) where

import Control.Applicative    ((<$>))
import Control.Concurrent     (forkIO, threadDelay)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, writeTVar)
import Control.Monad          (forever, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Char              (isAlphaNum)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Text              (Text, breakOn, takeEnd, toUpper, unpack)
import Data.Text.Encoding     (decodeUtf8)
import Data.Time.Calendar     (fromGregorian)
import Data.Time.Clock        (UTCTime(..), addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Format       (formatTime)
import Network
import Network.IRC            (Message, encode)
import Network.IRC.IDTE.Events (toEvent)
import Network.IRC.IDTE.Messages
import Network.IRC.IDTE.Types
import Network.TLS            (Cipher)
import System.Locale          (defaultTimeLocale)
import System.IO.Error        (catchIOError)

import qualified Data.Text            as T
import qualified Network.IRC.IDTE.Net as N

-- *Connecting to an IRC network

-- |Connect to a server without TLS.
connect :: MonadIO m => HostName -> Int -> m (Either String ConnectionConfig)
connect host port = do
  res <- N.connect host port

  return $
    case res of
      Right sock -> Right ConnectionConfig
                     { _socket = sock
                     , _tls    = Nothing
                     , _server = host
                     , _port   = port
                     , _disconnect = defaultDisconnectHandler
                     }
      Left err -> Left err

-- |Connect to a server with TLS.
connectWithTLS :: MonadIO m => HostName -> Int -> m (Either String ConnectionConfig)
connectWithTLS host port = connectWithTLS' host port N.defaultCiphers

-- |Connect to a server without TLS, supplying your own list of
-- ciphers, ordered by preference.
connectWithTLS' :: MonadIO m => HostName -> Int -> [Cipher] -> m (Either String ConnectionConfig)
connectWithTLS' host port ciphers = do
  res <- N.connectWithTLS' host port ciphers

  return $
    case res of
      Right (sock, ctx) -> Right ConnectionConfig
                            { _socket = sock
                            , _tls    = Just ctx
                            , _server = host
                            , _port   = port
                            , _disconnect = defaultDisconnectHandler
                            }
      Left err -> Left err

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
  -- Set the nick and username
  theNick <- _nick     <$> instanceConfig
  theUser <- _username <$> instanceConfig
  theReal <- _realname <$> instanceConfig

  send $ user theUser theReal
  send $ nick theNick

  -- Connect to channels
  mapM_ (send . join) . _channels <$> instanceConfig

  -- Run the event loop, and call the disconnect handler if the remote
  -- end closes the socket.
  state     <- ircState
  dchandler <- _disconnect <$> connectionConfig
  liftIO $ forever (runReaderT step state) `catchIOError` const (runReaderT dchandler state)

-- |One step of the event loop: block on receiving a message, attempt
-- to decode it, and invoke all matching handlers simultaneously.
step :: IRC ()
step = do
  msg <- N.recv
  case msg of
    Just msg' -> do
      logmsg True msg'

      event <- toEvent msg' send

      -- Get the current state
      state <- ircState

      -- And run every handler in parallel
      handlers <- getHandlersFor event . _eventHandlers <$> instanceConfig
      liftIO $ mapM_ (\h -> forkIO $ runReaderT (h event) state) handlers

    -- Ignore malformed messages
    Nothing -> return ()

-- |Get the event handlers for an event.
getHandlersFor :: Event -> [EventHandler] -> [Event -> IRC ()]
getHandlersFor e ehs = [_eventFunc eh | eh <- ehs, _matchType eh `elem` [EEverything, _eventType e]]

-- |Log a message to stdout and the internal log
logmsg :: Bool -> Message -> IRC ()
logmsg fromsrv msg = do
  now <- liftIO getCurrentTime

  liftIO . putStrLn $ unwords [ formatTime defaultTimeLocale "%c" now
                              , if fromsrv then "<---" else "--->"
                              , unpack . decodeUtf8 . encode $ msg
                              ]

-- *Messaging

-- |Send a message, using TLS if enabled. This blocks if messages are
-- sent too rapidly.
send :: Message -> IRC ()
send msg = do
  state <- ircState

  -- Send the message atomically.
  withLock . flip runReaderT state $ do
    -- Block until the flood delay passes
    now     <- liftIO getCurrentTime
    lastMsg <- _lastMessageTime <$> instanceConfig
    flood   <- fromIntegral . _floodDelay <$> instanceConfig

    let nextMsg = addUTCTime flood lastMsg
    when (nextMsg > now) $
      -- threadDelay uses microseconds, NominalDiffTime is in seconds,
      -- but with a precision of nanoseconds.
      liftIO . threadDelay . ceiling $ 1000000 * diffUTCTime nextMsg now

    -- Update the last message time
    ic   <- instanceConfig
    now' <- liftIO getCurrentTime
    putInstanceConfig ic { _lastMessageTime = now' }

    -- Send the message
    logmsg False msg
    N.send msg

-- *Disconnecting

-- |Disconnect from a server, properly tearing down the TLS session
-- (if there is one).
disconnect :: IRC ()
disconnect = N.disconnect

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
                     , _floodDelay    = 1
                     , _lastMessageTime = UTCTime (fromGregorian 0 0 0) 0
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
pingHandler :: Event -> IRC ()
pingHandler ev =
  case _message ev of
    Ping target -> send $ pong target
    _ -> return ()

-- |Respond to CTCP PINGs
ctcpPingHandler :: Event -> IRC ()
ctcpPingHandler ev =
  case (_source ev, _message ev) of
    (User n, CTCP p xs) | toUpper p == "PING" -> send $ ctcpReply n "PING" xs
    _ -> return ()

-- |Respond to CTCP VERSIONs
ctcpVersionHandler :: Event -> IRC ()
ctcpVersionHandler ev = do
  ver <- _ctcpVer <$> instanceConfig
  case (_source ev, _message ev) of
    (User n, CTCP v []) | toUpper v == "VERSION" -> send $ ctcpReply n "VERSION" [ver]
    _ -> return ()

-- |Respond to CTCP TIMEs
ctcpTimeHandler :: Event -> IRC ()
ctcpTimeHandler ev = do
  now <- liftIO getCurrentTime
  case (_source ev, _message ev) of
    (User n, CTCP t []) | toUpper t == "TIME" -> send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
    _ -> return ()

-- |Update the nick upon welcome, as it may not be what we requested
-- (eg, in the case of a nick too long).
welcomeNick :: Event -> IRC ()
welcomeNick ev = case _message ev of
                   Numeric 001 (srvNick:_) -> do
                     tvarI <- instanceConfigTVar

                     liftIO . atomically $ do
                       iconf <- readTVar tvarI
                       writeTVar tvarI iconf { _nick = srvNick }
                   _ -> return ()

-- |Mangle the nick if there's a collision when we set it
nickMangler :: Event -> IRC ()
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
joinHandler :: Event -> IRC ()
joinHandler ev = case _message ev of
                   Numeric 332 (c:_) -> do
                     tvarI <- instanceConfigTVar

                     liftIO . atomically $ do
                       iconf <- readTVar tvarI
                       unless (c `elem` _channels iconf) $
                         writeTVar tvarI iconf { _channels = c : _channels iconf }

                   _ -> return ()

-- |Update the channel list upon being kicked.
kickHandler :: Event -> IRC ()
kickHandler ev = do
  theNick <- _nick <$> instanceConfig
  tvarI   <- instanceConfigTVar

  case (_source ev, _message ev) of
    (Channel _ c, Kick n _) | n == theNick -> liftIO . atomically $ delChan tvarI c
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

  send $ nick new

-- |Update the channel list in the instance configuration and also
-- part the channel.
leaveChannel :: Text -> Maybe Text -> IRC ()
leaveChannel chan reason = do
  tvarI <- instanceConfigTVar
  liftIO . atomically $ delChan tvarI chan

  send $ part chan reason

-- |Remove a channel from the list.
delChan :: TVar InstanceConfig -> Text -> STM ()
delChan tvarI chan = do
  iconf <- readTVar tvarI
  writeTVar tvarI iconf { _channels = filter (/=chan) $ _channels iconf }

-- |Send a message to the source of an event.
reply :: Event -> Text -> IRC ()
reply ev txt = case _source ev of
                 Channel _ c -> send $ privmsg c txt
                 User n      -> send $ query n txt
                 _           -> return ()
