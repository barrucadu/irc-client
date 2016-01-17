-- |Commonly-used utility functions for IRC clients.
module Network.IRC.Client.Utils where

import Control.Concurrent.STM      (TVar, STM, atomically, readTVar, writeTVar)
import Control.Monad.IO.Class      (liftIO)
import Data.Text                   (Text)
import Network.IRC.CTCP            (toCTCP)
import Network.IRC.Client.Internal (send)
import Network.IRC.Client.Types

import qualified Data.Text as T

-- | Update the nick in the instance configuration and also send an
-- update message to the server. This doesn't attempt to resolve nick
-- collisions, that's up to the event handlers.
setNick :: Text -> StatefulIRC s ()
setNick new = do
  tvarI <- instanceConfigTVar

  liftIO . atomically $ do
    iconf <- readTVar tvarI
    writeTVar tvarI iconf { _nick = new }

  send $ Nick new

-- | Update the channel list in the instance configuration and also
-- part the channel.
leaveChannel :: Text -> Maybe Text -> StatefulIRC s ()
leaveChannel chan reason = do
  tvarI <- instanceConfigTVar
  liftIO . atomically $ delChan tvarI chan

  send $ Part chan reason

-- | Remove a channel from the list without sending a part command (be
-- careful not to let the channel list get out of sync with the
-- real-world state if you use it for anything!)
delChan :: TVar (InstanceConfig s) -> Text -> STM ()
delChan tvarI chan = do
  iconf <- readTVar tvarI
  writeTVar tvarI iconf { _channels = filter (/=chan) $ _channels iconf }

-- | Add an eventHandler
addHandler :: EventHandler () -> StatefulIRC () ()
addHandler handler = do
  tvarI <- instanceConfigTVar

  liftIO . atomically $  do
    iconf <- readTVar tvarI
    writeTVar tvarI iconf { _eventHandlers = handler : _eventHandlers iconf }


-- | Send a message to the source of an event.
reply :: UnicodeEvent -> Text -> StatefulIRC s ()
reply ev txt = case _source ev of
  Channel c _ -> mapM_ (send . Privmsg c . Right) $ T.lines txt
  User n      -> mapM_ (send . Privmsg n . Right) $ T.lines txt
  _           -> return ()

-- | Construct a @PRIVMSG@ containing a CTCP
ctcp :: Text -> Text -> [Text] -> UnicodeMessage
ctcp t command args = Privmsg t . Left $ toCTCP command args

-- | Construct a @NOTICE@ containing a CTCP
ctcpReply :: Text -> Text -> [Text] -> UnicodeMessage
ctcpReply t command args = Notice t . Left $ toCTCP command args
