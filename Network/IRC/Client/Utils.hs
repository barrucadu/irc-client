-- |Commonly-used utility functions for IRC clients.
module Network.IRC.Client.Utils where

import Control.Concurrent.STM      (TVar, STM, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class      (liftIO)
import Data.Text                   (Text)
import Network.IRC.CTCP            (toCTCP)
import Network.IRC.Client.Internal (send)
import Network.IRC.Client.Types

import qualified Data.Text as T

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
                 Channel c _ -> mapM_ (send . Privmsg c . Right) $ T.lines txt
                 User n      -> mapM_ (send . Privmsg n . Right) $ T.lines txt
                 _           -> return ()

-- |Construct a privmsg containing a CTCP
ctcp :: Text -> Text -> [Text] -> UnicodeMessage
ctcp t command args = Privmsg t . Left $ toCTCP command args

-- |Construct a notice containing a CTCP
ctcpReply :: Text -> Text -> [Text] -> UnicodeMessage
ctcpReply t command args = Notice t . Left $ toCTCP command args
