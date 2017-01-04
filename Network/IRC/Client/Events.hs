{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Network.IRC.Client.Events
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, OverloadedStrings
--
-- Events and event handlers. When a message is received from the
-- server, all matching handlers are executed concurrently.
module Network.IRC.Client.Events
  ( -- * Handlers
    EventHandler
  , eventHandler
  , matchCTCP
  , matchNumeric
  , matchType

  -- * Default handlers
  , defaultEventHandlers
  , defaultOnConnect
  , defaultOnDisconnect

  -- ** Individual handlers
  , pingHandler
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

import Control.Applicative    ((<$>))
import Control.Concurrent.STM (atomically, readTVar, modifyTVar)
import Control.Monad.IO.Class (liftIO)
import Data.Char              (isAlphaNum)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Text              (Text, breakOn, takeEnd, toUpper)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Format       (formatTime)
import Network.IRC.Conduit    (Event(..), Message(..), Source(..))
import Network.IRC.CTCP       (fromCTCP)
import Network.IRC.Conduit.Lens


#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale    (defaultTimeLocale)
#endif

import qualified Data.Text as T

import Network.IRC.Client.Internal
import Network.IRC.Client.Lens
import Network.IRC.Client.Utils


-------------------------------------------------------------------------------
-- Handlers

-- | An event handler has two parts: a predicate to determine if the
-- handler matches the event, and the function to invoke if so.
eventHandler
  :: (Event Text -> Bool)
  -- ^ Event matching predicate
  -> (Event Text -> Irc s ())
  -- ^ Event handler.
  -> EventHandler s
eventHandler = EventHandler

-- | Match the verb of a CTCP, ignoring case.
--
-- > matchCTCP "ping"   ":foo PRIVMSG #bar :\001PING\001"          ==> True
-- > matchCTCP "PING"   ":foo PRIVMSG #bar :\001PING\001"          ==> True
-- > matchCTCP "ACTION" ":foo PRIVMSG #bar :\001ACTION dances\001" ==> True
matchCTCP :: Text -> Event Text -> Bool
matchCTCP verb ev = case _message ev of
  Privmsg _ (Left ctcpbs) ->
    let (v, _) = fromCTCP ctcpbs
    in toUpper verb == toUpper v
  _ -> False

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
matchNumeric :: Int -> Event a -> Bool
matchNumeric num ev = case _message ev of
  Numeric n _ -> num == n
  _ -> False

-- | A simple predicate to match events of the given type. Refer to
-- "Network.IRC.Conduit.Lens#Message" for the list of 'Prism''s.
--
-- > matchType _Privmsg ":foo PRIVMSG #bar :hello world" ==> True
-- > matchType _Quit    ":foo QUIT :goodbye world"       ==> True
matchType :: Prism' (Message a) b -> Event a -> Bool
matchType k = is k . _message


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
defaultOnConnect :: Irc s ()
defaultOnConnect = do
  iconf <- snapshot instanceConfig =<< getIrcState
  send . Nick $ get nick iconf

-- | The default disconnect handler: do nothing. You might want to
-- override this with one which reconnects.
defaultOnDisconnect :: Irc s ()
defaultOnDisconnect = return ()


-------------------------------------------------------------------------------
-- Individual handlers

-- | Respond to server @PING@ messages with a @PONG@.
pingHandler :: EventHandler s
pingHandler = eventHandler (matchType _Ping) $ \ev -> case _message ev of
  Ping s1 s2 -> send . Pong $ fromMaybe s1 s2
  _ -> return ()

-- | Respond to CTCP @PING@ requests.
ctcpPingHandler :: EventHandler s
ctcpPingHandler = eventHandler (matchCTCP "PING") $ \ev -> case (_source ev, _message ev) of
  (User n, Privmsg _ (Left ctcpbs)) ->
    let (_, xs) = fromCTCP ctcpbs
    in send $ ctcpReply n "PING" xs
  _ -> pure ()

-- | Respond to CTCP @VERSION@ requests with the version string.
ctcpVersionHandler :: EventHandler s
ctcpVersionHandler = eventHandler (matchCTCP "VERSION") $ \ev -> case _source ev of
  User n -> do
    ver <- get version <$> (snapshot instanceConfig =<< getIrcState)
    send $ ctcpReply n "VERSION" [ver]
  _ -> pure ()

-- | Respond to CTCP @TIME@ requests with the system time.
ctcpTimeHandler :: EventHandler s
ctcpTimeHandler = eventHandler (matchCTCP "TIME") $ \ev -> case _source ev of
  User n -> do
    now <- liftIO getCurrentTime
    send $ ctcpReply n "TIME" [T.pack $ formatTime defaultTimeLocale "%c" now]
  _ -> pure ()

-- | Update the nick upon welcome (numeric reply 001), as it may not
-- be what we requested (eg, in the case of a nick too long).
welcomeNick :: EventHandler s
welcomeNick = eventHandler (matchNumeric 001) $ \ev -> case _message ev of
    Numeric _ xs ->  go xs
    _ -> pure ()
  where
    go (srvNick:_) = do
      tvarI <- get instanceConfig <$> getIrcState
      liftIO . atomically $
        modifyTVar tvarI (set nick srvNick)
    go _ = pure ()

-- | Join default channels upon welcome (numeric reply 001). If sent earlier,
-- the server might reject the JOIN attempts.
joinOnWelcome :: EventHandler s
joinOnWelcome = eventHandler (matchNumeric 001) $ \_ -> do
  iconf <- snapshot instanceConfig =<< getIrcState
  mapM_ (send . Join) $ get channels iconf

-- | Mangle the nick if there's a collision (numeric replies 432, 433,
-- and 436) when we set it
nickMangler :: EventHandler s
nickMangler = eventHandler (\ev -> any (($ev) . matchNumeric) [432, 433, 436]) $ \ev -> case _message ev of
    Numeric 432 xs -> go fresh xs
    Numeric 433 xs -> go mangle xs
    Numeric 436 xs -> go mangle xs
    _ -> pure ()
  where
    go f (_:srvNick:_) = do
      theNick <- get nick <$> (snapshot instanceConfig =<< getIrcState)

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
joinHandler = eventHandler (\ev -> matchNumeric 331 ev || matchNumeric 332 ev) $ \ev -> case _message ev of
    Numeric _ xs -> go xs
    _ -> pure ()
  where
    go (c:_) = do
      tvarI <- get instanceConfig <$> getIrcState
      liftIO . atomically $
        modifyTVar tvarI $ \iconf ->
          (if c `elem` get channels iconf
            then modify channels (c:)
            else id) iconf

    go _ = pure ()

-- | Update the channel list upon being kicked.
kickHandler :: EventHandler s
kickHandler = eventHandler (matchType _Kick) $ \ev -> do
  tvarI <- get instanceConfig <$> getIrcState
  liftIO . atomically $ do
    theNick <- get nick <$> readTVar tvarI
    case (_source ev, _message ev) of
      (Channel c _, Kick n _ _) | n == theNick -> delChan tvarI c
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
