{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- | 'Lens'es.
module Network.IRC.Client.Lens where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Conduit (Consumer, Producer)
import Data.Conduit.TMChan (TBMChan)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.IRC.Conduit

import Network.IRC.Client.Types.Internal

-- CPP seem to dislike the first ' on the RHS…
#define PRIME() '

#define LENS(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Lens' for '_/**/F'. -}; \
    F :: Lens' S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)

-- * Lenses for 'IRCState'
LENS((IRCState s),connectionConfig,(ConnectionConfig s))
LENS((IRCState s),userState,(TVar s))
LENS((IRCState s),instanceConfig,(TVar (InstanceConfig s)))
LENS((IRCState s),connectionState,(TVar ConnectionState))

-- * Lenses for 'ConnectionConfig'
LENS((ConnectionConfig s),func,(IO () -> Consumer (Either ByteString IrcEvent) IO () -> Producer IO IrcMessage -> IO ()))
LENS((ConnectionConfig s),sendqueue,(TBMChan IrcMessage))
LENS((ConnectionConfig s),server,ByteString)
LENS((ConnectionConfig s),port,Int)
LENS((ConnectionConfig s),username,Text)
LENS((ConnectionConfig s),realname,Text)
LENS((ConnectionConfig s),password,(Maybe Text))
LENS((ConnectionConfig s),flood,NominalDiffTime)
LENS((ConnectionConfig s),timeout,NominalDiffTime)
LENS((ConnectionConfig s),onconnect,(IRC s ()))
LENS((ConnectionConfig s),ondisconnect,(IRC s ()))
LENS((ConnectionConfig s),logfunc,(Origin -> ByteString -> IO ()))

-- * Lenses for 'InstanceConfig'
LENS((InstanceConfig s),nick,Text)
LENS((InstanceConfig s),channels,[Text])
LENS((InstanceConfig s),version,Text)
LENS((InstanceConfig s),handlers,[EventHandler s])
LENS((InstanceConfig s),ignore,[(Text, Maybe Text)])

-- * Lenses for 'EventHandler'
LENS((EventHandler s),eventPred,(Event Text -> Bool))
LENS((EventHandler s),eventFunc,(Event Text -> IRC s ()))

