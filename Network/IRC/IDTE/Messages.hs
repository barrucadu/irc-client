{-# LANGUAGE OverloadedStrings #-}

-- |Constructors for common messages
module Network.IRC.IDTE.Messages where

import Prelude hiding (concat)

import Data.ByteString    (ByteString)
import Data.Monoid        ((<>))
import Data.String        (fromString)
import Data.Text          (Text, concat)
import Data.Text.Encoding (encodeUtf8)
import Network.IRC        (Message(..))
import Network.IRC.CTCP   (getUnderlyingByteString, toCTCP)
import Network.IRC.IDTE.Utils

-- *Messages

-- |Send a privmsg to a channel
privmsg :: Text
        -- ^The target
        -> Text
    -- ^The message
        -> Message
privmsg c m = mkMessage "PRIVMSG" >$: [c, m]

-- |Send a privmsg to a user
query :: Text -> Text -> Message
query n m = mkMessage "PRIVMSG" >$: [n, m]

-- |Send a notice to a channel or a user
notice :: Text -> Text -> Message
notice t m = mkMessage "NOTICE" >$: [t, m]

-- |Send a CTCP to a channel or a user
ctcp :: Text
     -- ^The target
     -> Text
    -- ^The command
     -> [Text]
    -- ^The arguments
     -> Message
ctcp t cmd args = mkMessage "PRIVMSG" [encodeUtf8 t, getUnderlyingByteString $ toCTCP cmd args]

-- |Reply to a CTCP. Like 'ctcp', but sends a NOTICE.
ctcpReply :: Text -> Text -> [Text] -> Message
ctcpReply t cmd args = mkMessage "NOTICE" [encodeUtf8 t, getUnderlyingByteString $ toCTCP cmd args]

-- |Set your nick
nick :: Text -> Message
nick n = mkMessage "NICK" >$: [n]

-- |Set your username and realname
user :: Text -> Text -> Message
user u r = mkMessage "USER" >$: [u, "-", "-", r]

-- |Join a channel
join :: Text -> Message
join c = mkMessage "JOIN" >$: [c]

-- |Leave a channel
part :: Text -> Maybe Text -> Message
part c (Just r) = mkMessage "PART" >$: [c, r]
part c Nothing  = mkMessage "PART" >$: [c]

-- |Quit IRC
quit :: Maybe Text -> Message
quit (Just r) = mkMessage "QUIT" >$: [r]
quit Nothing  = mkMessage "QUIT" []

-- |Kick someone from a channel
kick :: Text
     -- ^The channel
     -> Text
    -- ^The nick
     -> Maybe Text
    -- ^The reason
     -> Message
kick c n (Just r) = mkMessage "KICK" >$: [c, n, r]
kick c n Nothing  = mkMessage "KICK" >$: [c, n]

-- |Set modes on a user or channel
mode :: Text
     -- ^Target
     -> Bool
    -- ^Enable/disable
     -> [Text]
    -- ^List of flags
     -> [Text]
    -- ^Any params
     -> Message
mode n True  fs ps = mkMessage "MODE" >$: (n : ("+" <> concat fs) : ps)
mode n False fs ps = mkMessage "MODE" >$: (n : ("-" <> concat fs) : ps)

-- |Set the topic of a channel
topic :: Text -> Text -> Message
topic c t = mkMessage "TOPIC" >$: [c, t]

-- |Invite someone to a channel
invite :: Text
       -- ^The nick
       -> Text
    -- ^The channel
       -> Message
invite n c = mkMessage "INVITE" >$: [n, c]

-- |Send the server a PING message
ping :: Text -> Message
ping t = mkMessage "PING" >$: [t]

-- |Send the server a PONG message
pong :: Text -> Message
pong t = mkMessage "PONG" >$: [t]

-- |Send the server a numeric response
numeric :: Int -> [Text] -> Message
numeric i args = mkMessage (fromString $ show i) >$: args

-- |Build a message from parts
mkMessage :: ByteString -> [ByteString] -> Message
mkMessage = Message Nothing

