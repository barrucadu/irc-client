{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Events
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings, RankNTypes
--
-- Events and event handlers. When a message is received from the
-- server, all matching handlers are executed sequentially in the
-- order that they appear in the 'handlers' list.
module Network.IRC.Client.Events
  ( -- * Handlers
    EventHandler(..)
  , matchCTCP
  , matchNumeric
  , matchType
  , matchWhen

  -- * Default handlers
  , defaultEventHandlers
  , defaultOnConnect
  , defaultOnDisconnect

  -- ** Individual handlers
  , pingHandler
  , kickHandler
  , ctcpPingHandler
  , ctcpVersionHandler
  , ctcpTimeHandler
  , welcomeNick
  , joinOnWelcome
  , joinHandler
  , nickMangler

  -- * Re-exported
  , Event(..)
  , Message(..)
  , Source(..)
  , module Network.IRC.Conduit.Lens
  ) where

import           Control.Applicative         ((<$>), (<|>))
import           Control.Concurrent.STM      (atomically, modifyTVar, readTVar)
import           Control.Monad.Catch         (SomeException, fromException,
                                              throwM)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Char                   (isAlphaNum)
import           Data.Maybe                  (fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text, breakOn, takeEnd, toUpper)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Time.Format            (formatTime)
import           Network.IRC.Conduit         (Event(..), Message(..),
                                              Source(..))
import           Network.IRC.Conduit.Lens
import           Network.IRC.CTCP            (fromCTCP)

#if MIN_VERSION_time(1,5,0)
import           Data.Time.Format            (defaultTimeLocale)
#else
import           System.Locale               (defaultTimeLocale)
#endif

import qualified Data.Text                   as T

import           Network.IRC.Client.Internal
import           Network.IRC.Client.Lens
import           Network.IRC.Client.Utils


-------------------------------------------------------------------------------
-- Handlers

-- | Match the verb of a CTCP, ignoring case, and returning the arguments.
--
-- > matchCTCP "ping"   ":foo PRIVMSG #bar :\001PING\001"          ==> Just []
-- > matchCTCP "PING"   ":foo PRIVMSG #bar :\001PING\001"          ==> Just []
-- > matchCTCP "ACTION" ":foo PRIVMSG #bar :\001ACTION dances\001" ==> Just ["dances"]
matchCTCP :: Text -> Event Text -> Maybe [Text]
matchCTCP verb ev = case _message ev of
  Privmsg _ (Left ctcpbs) ->
    let (v, args) = fromCTCP ctcpbs
    in if toUpper verb == toUpper v
       then Just args
       else Nothing
  _ -> Nothing

-- | Match a numeric server message. Numeric messages are sent in
-- response to most things, such as connecting to the server, or
-- joining a channel.
--
-- Numerics in the range 001 to 099 are informative messages, numerics
-- in the range 200 to 399 are responses to commands. Some common
-- numerics are:
--
--    - 001 (RPL_WELCOME), sent after successfully connecting.
--
--    - 331 (RPL_NOTOPIC), sent after joining a channel if it has no
--      topic.
--
--    - 332 (RPL_TOPIC), sent after joining a channel if it has a
--      topic.
--
--    - 432 (ERR_ERRONEUSNICKNAME), sent after trying to change to an
--      invalid nick.
--
--    - 433 (ERR_NICKNAMEINUSE), sent after trying to change to a nick
--      already in use.
--
--    - 436 (ERR_NICKCOLLISION), sent after trying to change to a nick
--      in use on another server.
--
-- See Section 5 of @<https://tools.ietf.org/html/rfc2812#section-5
-- RFC 2812>@ for a complete list.
--
-- > matchNumeric 001 "001 :Welcome to irc.example.com" ==> True
-- > matchNumeric 332 "332 :#haskell: We like Haskell"  ==> True
matchNumeric :: Int -> Event a -> Maybe [a]
matchNumeric num ev = case _message ev of
  Numeric n args | num == n -> Just args
  _ -> Nothing

-- | Match events of the given type. Refer to
-- "Network.IRC.Conduit.Lens#Message" for the list of 'Prism''s.
--
-- > matchType _Privmsg ":foo PRIVMSG #bar :hello world" ==> Just ("#bar", Right "hello world")
-- > matchType _Quit    ":foo QUIT :goodbye world"       ==> Just (Just "goodbye world")
matchType :: Prism' (Message a) b -> Event a -> Maybe b
matchType k = preview k . _message

-- | Match a predicate against an event.
--
-- > matchWhen (const True) ":foo PRIVMSG #bar :hello world" ==> Just ":foo PRIVMSG :hello world"
matchWhen :: (Event a -> Bool) -> Event a -> Maybe (Message a)
matchWhen p ev | p ev = Just (_message ev)
matchWhen _ _ = Nothing


-------------------------------------------------------------------------------
-- Default handlers

-- | The default event handlers, the following are included:
--
-- - respond to server @PING@ messages with a @PONG@;
-- - respond to CTCP @PING@ requests;
-- - respond to CTCP @VERSION@ requests with the version string;
-- - respond to CTCP @TIME@ requests with the system time;
-- - update the nick upon receiving the welcome message, in case the
--   server modifies it;
-- - mangle the nick if the server reports a collision;
-- - update the channel list on @JOIN@ and @KICK@.
defaultEventHandlers :: [EventHandler s]
defaultEventHandlers =
  [ pingHandler
  , kickHandler
  , ctcpPingHandler
  , ctcpTimeHandler
  , ctcpVersionHandler
  , welcomeNick
  , joinOnWelcome
  , joinHandler
  , nickMangler
  ]

-- | The default connect handler: set the nick.
defaultOnConnect :: IRC s ()
defaultOnConnect = do
  iconf <- snapshot instanceConfig =<< getIRCState
  send . Nick $ get nick iconf

-- | The default disconnect handler
--
--    - If the client disconnected due to a 'Timeout' exception, reconnect.
--
--    - If the client disconnected due to another exception, rethrow it.
--
--    - If the client disconnected without an exception, halt.
defaultOnDisconnect :: Maybe SomeException -> IRC s ()
defaultOnDisconnect (Just exc) = case fromException exc of
  Just Timeout -> reconnect
  Nothing -> throwM exc
defaultOnDisconnect Nothing = pure ()


-------------------------------------------------------------------------------
-- Individual handlers

-- | Respond to server @PING@ messages with a @PONG@.
pingHandler :: EventHandler s
pingHandler = EventHandler (matchType _Ping) $ \_ (s1, s2) ->
  send . Pong $ fromMaybe s1 s2

-- | Respond to CTCP @PING@ requests.
ctcpPingHandler :: EventHandler s
ctcpPingHandler = EventHandler (matchCTCP "PING") $ \src args -> case src of
  User n -> send $ ctcpReply n "PING" args
  _ -> pure ()

-- | Respond to CTCP @VERSION@ requests with the version string.
ctcpVersionHandler :: EventHandler s
ctcpVersionHandler = EventHandler (matchCTCP "VERSION") $ \src _ -> case src of
  User n -> do
    ver <- get version <$> (snapshot instanceConfig =<< getIRCState)
    send $ ctcpReply n "VERSION" [ver]
  _ -> pure ()

-- | Respond to CTCP @TIME@ requests with the system time.
ctcpTimeHandler :: EventHandler s
ctcpTimeHandler = EventHandler (matchCTCP "TIME") $ \src _ -> case src of
  User n -> do
    now <- liftIO getCurrentTime
    send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
  _ -> pure ()

-- | Update the nick upon welcome (numeric reply 001), as it may not
-- be what we requested (eg, in the case of a nick too long).
welcomeNick :: EventHandler s
welcomeNick = EventHandler (matchNumeric 001) $ \_ args -> case args of
  (srvNick:_) -> do
    tvarI <- get instanceConfig <$> getIRCState
    liftIO . atomically $
      modifyTVar tvarI (set nick srvNick)
  [] -> pure ()

-- | Join default channels upon welcome (numeric reply 001). If sent earlier,
-- the server might reject the JOIN attempts.
joinOnWelcome :: EventHandler s
joinOnWelcome = EventHandler (matchNumeric 001) $ \_ _ -> do
  iconf <- snapshot instanceConfig =<< getIRCState
  mapM_ (send . Join) $ get channels iconf

-- | Mangle the nick if there's a collision (numeric replies 432, 433,
-- and 436) when we set it
nickMangler :: EventHandler s
nickMangler = EventHandler (\ev -> matcher 432 fresh ev <|> matcher 433 mangle ev <|> matcher 436 mangle ev) $ \_ -> uncurry go
  where
    matcher num f ev = case _message ev of
      Numeric n args | num == n -> Just (f, args)
      _ -> Nothing

    go f (_:srvNick:_) = do
      theNick <- get nick <$> (snapshot instanceConfig =<< getIRCState)

      -- If the length of our nick and the server's idea of our nick
      -- differ, it was truncated - so calculate the allowable length.
      let nicklen = if T.length srvNick /= T.length theNick
                    then Just $ T.length srvNick
                    else Nothing

      setNick . trunc nicklen $ f srvNick
    go _ _ = return ()

    fresh n = if T.length n' == 0 then "f" else n'
      where n' = T.filter isAlphaNum n

    mangle n = (n <> "1") `fromMaybe` charsubst n

    -- Truncate a nick, if there is a known length limit.
    trunc len txt = maybe txt (`takeEnd` txt) len

    -- List of substring substitutions. It's important that these
    -- don't contain any loops!
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
      _ -> transform trs txt
    transform [] _ = Nothing

-- | Upon joining a channel (numeric reply 331 or 332), add it to the
-- list (if not already present).
joinHandler :: EventHandler s
joinHandler = EventHandler (\ev -> matchNumeric 331 ev <|> matchNumeric 332 ev) $ \_ args -> case args of
  (c:_) -> do
    tvarI <- get instanceConfig <$> getIRCState
    liftIO . atomically $
      modifyTVar tvarI $ \iconf ->
        (if c `elem` get channels iconf
          then modify channels (c:)
          else id) iconf
  _ -> pure ()

-- | Update the channel list upon being kicked.
kickHandler :: EventHandler s
kickHandler = EventHandler (matchType _Kick) $ \src (n, _, _) -> do
  tvarI <- get instanceConfig <$> getIRCState
  liftIO . atomically $ do
    theNick <- get nick <$> readTVar tvarI
    case src of
      Channel c _
        | n == theNick -> delChan tvarI c
        | otherwise    -> pure ()
      _ -> pure ()


-------------------------------------------------------------------------------
-- Utils

-- | Break some text on the first occurrence of a substring, removing
-- the substring from the second portion.
breakOn' :: Text -> Text -> Maybe (Text, Text)
breakOn' delim txt = if T.length after >= T.length delim
                     then Just (before, T.drop (T.length delim) after)
                     else Nothing
  where
    (before, after) = breakOn delim txt
