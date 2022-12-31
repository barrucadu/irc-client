-- |
-- Module      : Network.IRC.Client.Utils
-- Copyright   : (c) 2016 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : portable
--
-- Commonly-used utility functions for IRC clients.
module Network.IRC.Client.Utils
  ( -- * Nicks
    setNick

    -- * Channels
  , leaveChannel
  , delChan

    -- * Events
  , addHandler
  , reply
  , replyTo

    -- * CTCPs
  , ctcp
  , ctcpReply

    -- * Connection state
  , isConnected
  , isDisconnecting
  , isDisconnected
  , snapConnState

    -- * Concurrency
  , fork

    -- * Lenses
  , snapshot
  , snapshotModify
  , get
  , set
  , modify
  ) where

import           Control.Concurrent          (ThreadId, forkFinally, myThreadId)
import           Control.Concurrent.STM      (STM, TVar, atomically, modifyTVar)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Network.IRC.Conduit         (Event(..), Message(..),
                                              Source(..))
import           Network.IRC.CTCP            (toCTCP)

import           Network.IRC.Client.Internal
import           Network.IRC.Client.Lens

-------------------------------------------------------------------------------
-- Nicks

-- | Update the nick in the instance configuration and also send an
-- update message to the server. This doesn't attempt to resolve nick
-- collisions, that's up to the event handlers.
setNick :: Text -> IRC s ()
setNick new = do
  tvarI <- get instanceConfig <$> getIRCState
  liftIO . atomically $
    modifyTVar tvarI (set nick new)
  send $ Nick new


-------------------------------------------------------------------------------
-- Channels

-- | Update the channel list in the instance configuration and also
-- part the channel.
leaveChannel :: Text -> Maybe Text -> IRC s ()
leaveChannel chan reason = do
  tvarI <- get instanceConfig <$> getIRCState
  liftIO . atomically $ delChan tvarI chan
  send $ Part chan reason

-- | Remove a channel from the list without sending a part command (be
-- careful not to let the channel list get out of sync with the
-- real-world state if you use it for anything!)
delChan :: TVar (InstanceConfig s) -> Text -> STM ()
delChan tvarI chan =
  modifyTVar tvarI (modify channels (filter (/=chan)))


-------------------------------------------------------------------------------
-- Events

-- | Add an event handler
addHandler :: EventHandler s -> IRC s ()
addHandler handler = do
  tvarI <- get instanceConfig <$> getIRCState
  liftIO . atomically $
    modifyTVar tvarI (modify handlers (handler:))

-- | Send a message to the source of an event.
reply :: Event Text -> Text -> IRC s ()
reply = replyTo . _source

-- | Send a message to the source of an event.
replyTo :: Source Text -> Text -> IRC s ()
replyTo (Channel c _) = mapM_ (send . Privmsg c . Right) . T.lines
replyTo (User n)      = mapM_ (send . Privmsg n . Right) . T.lines
replyTo _ = const $ pure ()


-------------------------------------------------------------------------------
-- CTCPs

-- | Construct a @PRIVMSG@ containing a CTCP
ctcp :: Text -> Text -> [Text] -> Message Text
ctcp t command args = Privmsg t . Left $ toCTCP command args

-- | Construct a @NOTICE@ containing a CTCP
ctcpReply :: Text -> Text -> [Text] -> Message Text
ctcpReply t command args = Notice t . Left $ toCTCP command args


-------------------------------------------------------------------------------
-- Connection state

-- | Check if the client is connected.
isConnected :: IRC s Bool
isConnected = (==Connected) <$> snapConnState

-- | Check if the client is in the process of disconnecting.
isDisconnecting :: IRC s Bool
isDisconnecting = (==Disconnecting) <$> snapConnState

-- | Check if the client is disconnected
isDisconnected :: IRC s Bool
isDisconnected = (==Disconnected) <$> snapConnState

-- | Snapshot the connection state.
snapConnState :: IRC s ConnectionState
snapConnState = liftIO . atomically . getConnectionState =<< getIRCState


-------------------------------------------------------------------------------
-- Concurrency

-- | Fork a thread which will be thrown a 'Disconnect' exception when
-- the client disconnects.
fork :: IRC s () -> IRC s ThreadId
fork ma = do
  s <- getIRCState
  liftIO $ do
    tid <- forkFinally (runIRCAction ma s) $ \_ -> do
      tid <- myThreadId
      atomically $ modifyTVar (_runningThreads s) (S.delete tid)
    atomically $ modifyTVar (_runningThreads s) (S.insert tid)
    pure tid
