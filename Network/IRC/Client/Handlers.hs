{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The default event handlers. Handlers are invoked concurrently
-- when matching events are received from the server.
module Network.IRC.Client.Handlers
  ( -- * Event handlers
    defaultEventHandlers
  , pingHandler
  , ctcpPingHandler
  , ctcpVersionHandler
  , ctcpTimeHandler
  , welcomeNick
  , nickMangler

  -- * Special handlers
  , defaultOnConnect
  , defaultOnDisconnect
  ) where

import Control.Applicative    ((<$>))
import Control.Arrow          (first)
import Control.Concurrent.STM (atomically, readTVar, writeTVar)
import Control.Monad          (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Char              (isAlphaNum)
import Data.Maybe             (fromMaybe)
import Data.Monoid            ((<>))
import Data.Text              (Text, breakOn, takeEnd, toUpper)
import Data.Time.Clock        (getCurrentTime)
import Data.Time.Format       (formatTime)
import Network.IRC.CTCP       (fromCTCP)
import Network.IRC.Client.Types
import Network.IRC.Client.Utils
import Network.IRC.Client.Internal

#if MIN_VERSION_time(1,5,0)
import Data.Time.Format (defaultTimeLocale)
#else
import System.Locale    (defaultTimeLocale)
#endif

import qualified Data.Text as T

-- * Event handlers

-- | The default event handlers, the following are included:
--
-- - respond to server @PING@ messages with a @PONG@;
-- - respond to CTCP @PING@ requests with a CTCP @PONG@;
-- - respond to CTCP @VERSION@ requests with the version string;
-- - respond to CTCP @TIME@ requests with the system time;
-- - update the nick upon receiving the welcome message, in case the
--   server modifies it;
-- - mangle the nick if the server reports a collision;
-- - update the channel list on @JOIN@ and @KICK@.
--
-- These event handlers are all exposed through the
-- Network.IRC.Client.Handlers module, so you can use them directly if
-- you are building up your 'InstanceConfig' from scratch.
--
-- If you are building a bot, you may want to write an event handler
-- to process messages representing commands.
defaultEventHandlers :: [EventHandler s]
defaultEventHandlers =
  [ EventHandler "Respond to server PING requests"  EPing    pingHandler
  , EventHandler "Respond to CTCP PING requests"    ECTCP    ctcpPingHandler
  , EventHandler "Respond to CTCP VERSION requests" ECTCP    ctcpVersionHandler
  , EventHandler "Respond to CTCP TIME requests"    ECTCP    ctcpTimeHandler
  , EventHandler "Update the nick upon welcome"     ENumeric welcomeNick
  , EventHandler "Mangle the nick on collision"     ENumeric nickMangler
  , EventHandler "Update the channel list on JOIN"  ENumeric joinHandler
  , EventHandler "Update the channel lift on KICK"  EKick    kickHandler
  ]

-- | Respond to server @PING@ messages with a @PONG@.
pingHandler :: UnicodeEvent -> StatefulIRC s ()
pingHandler ev = case _message ev of
  Ping s1 s2 -> send . Pong $ fromMaybe s1 s2
  _ -> return ()

-- | Respond to CTCP @PING@ requests with a CTCP @PONG@.
ctcpPingHandler :: UnicodeEvent -> StatefulIRC s ()
ctcpPingHandler = ctcpHandler [("PING", return)]

-- | Respond to CTCP @VERSION@ requests with the version string.
ctcpVersionHandler :: UnicodeEvent -> StatefulIRC s ()
ctcpVersionHandler = ctcpHandler [("VERSION", go)] where
  go _ = do
    ver <- _ctcpVer <$> instanceConfig
    return [ver]

-- | Respond to CTCP @TIME@ requests with the system time.
ctcpTimeHandler :: UnicodeEvent -> StatefulIRC s ()
ctcpTimeHandler = ctcpHandler [("TIME", go)] where
  go _ = do
    now <- liftIO getCurrentTime
    return [T.pack $ formatTime defaultTimeLocale "%c" now]

-- | Update the nick upon welcome (numeric reply 001), as it may not
-- be what we requested (eg, in the case of a nick too long).
welcomeNick :: UnicodeEvent -> StatefulIRC s ()
welcomeNick = numHandler [(001, go)] where
  go (srvNick:_) = do
    tvarI <- instanceConfigTVar

    liftIO . atomically $ do
      iconf <- readTVar tvarI
      writeTVar tvarI iconf { _nick = srvNick }
  go _ = return ()

-- | Mangle the nick if there's a collision (numeric replies 432, 433,
-- and 436) when we set it
nickMangler :: UnicodeEvent -> StatefulIRC s ()
nickMangler = numHandler [ (432, go fresh)
                         , (433, go mangle)
                         , (436, go mangle)
                         ]
  where
    go f (_:srvNick:_) = do
      theNick <- _nick <$> instanceConfig

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

-- | Upon receiving a channel topic (numeric reply 332), add the
-- channel to the list (if not already present).
joinHandler :: UnicodeEvent -> StatefulIRC s ()
joinHandler = numHandler [(332, go)] where
  go (c:_) = do
    tvarI <- instanceConfigTVar

    liftIO . atomically $ do
      iconf <- readTVar tvarI
      unless (c `elem` _channels iconf) $
        writeTVar tvarI iconf { _channels = c : _channels iconf }

  go _ = return ()

-- | Update the channel list upon being kicked.
kickHandler :: UnicodeEvent -> StatefulIRC s ()
kickHandler ev = do
  theNick <- _nick <$> instanceConfig
  tvarI   <- instanceConfigTVar

  case (_source ev, _message ev) of
    (Channel c _, Kick n _ _) | n == theNick -> liftIO . atomically $ delChan tvarI c
                              | otherwise   -> return ()
    _ -> return ()

-- *Special

-- | The default connect handler: set the nick and join default
-- channels.
defaultOnConnect :: StatefulIRC s ()
defaultOnConnect = do
  iconf <- instanceConfig
  send . Nick $ _nick iconf
  mapM_ (send . Join) $ _channels iconf

-- | The default disconnect handler: do nothing. You might want to
-- override this with one which reconnects.
defaultOnDisconnect :: StatefulIRC s ()
defaultOnDisconnect = return ()

-- *Utils

-- | Match and handle a named CTCP
ctcpHandler :: [(Text, [Text] -> StatefulIRC s [Text])] -> UnicodeEvent -> StatefulIRC s ()
ctcpHandler hs ev = case (_source ev, _message ev) of
  (User n, Privmsg _ (Left ctcpbs)) ->
    let (verb, xs) = first toUpper $ fromCTCP ctcpbs
    in case lookup verb hs of
         Just f -> do
           args <- f xs
           send $ ctcpReply n verb args
         _ -> return ()
  _ -> return ()

-- | Match and handle a numeric reply
numHandler :: [(Int, [Text] -> StatefulIRC s ())] -> UnicodeEvent -> StatefulIRC s ()
numHandler hs ev = case _message ev of
  Numeric num xs -> maybe (return ()) ($xs) $ lookup num hs
  _ -> return ()

-- | Break some text on the first occurrence of a substring, removing
-- the substring from the second portion.
breakOn' :: Text -> Text -> Maybe (Text, Text)
breakOn' delim txt = if T.length after >= T.length delim
                     then Just (before, T.drop (T.length delim) after)
                     else Nothing
  where
    (before, after) = breakOn delim txt
