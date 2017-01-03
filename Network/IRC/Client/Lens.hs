{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- |
-- Module      : Network.IRC.Client.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP, ImpredicativeTypes
--
-- Lenses.
module Network.IRC.Client.Lens where

import Control.Concurrent.STM (TVar)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Network.IRC.Conduit

import Network.IRC.Client.Internal.Lens
import Network.IRC.Client.Internal.Types

{-# ANN module ("HLint: ignore Redundant lambda") #-}

-- CPP seem to dislike the first ' on the RHS…
#define PRIME() '

#define LENS(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Lens' for '_/**/F'. -}; \
    F :: Lens' S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)

#define GETTER(S,F,A) \
    {-# INLINE F #-}; \
    {-| PRIME()Getter' for '_/**/F'. -}; \
    F :: Getter S A; \
    F = \ afb s -> (\ b -> s {_/**/F = b}) <$> afb (_/**/F s)


-------------------------------------------------------------------------------
-- * Lenses for 'IRCState'

GETTER((IRCState s),connectionConfig,(ConnectionConfig s))
LENS((IRCState s),userState,(TVar s))
LENS((IRCState s),instanceConfig,(TVar (InstanceConfig s)))
LENS((IRCState s),connectionState,(TVar ConnectionState))


-------------------------------------------------------------------------------
-- * Lenses for 'ConnectionConfig'

GETTER((ConnectionConfig s),server,ByteString)
GETTER((ConnectionConfig s),port,Int)
LENS((ConnectionConfig s),username,Text)
LENS((ConnectionConfig s),realname,Text)
LENS((ConnectionConfig s),password,(Maybe Text))
LENS((ConnectionConfig s),flood,NominalDiffTime)
LENS((ConnectionConfig s),timeout,NominalDiffTime)
LENS((ConnectionConfig s),onconnect,(IRC s ()))
LENS((ConnectionConfig s),ondisconnect,(IRC s ()))
LENS((ConnectionConfig s),logfunc,(Origin -> ByteString -> IO ()))


-------------------------------------------------------------------------------
-- * Lenses for 'InstanceConfig'

LENS((InstanceConfig s),nick,Text)
LENS((InstanceConfig s),channels,[Text])
LENS((InstanceConfig s),version,Text)
LENS((InstanceConfig s),handlers,[EventHandler s])
LENS((InstanceConfig s),ignore,[(Text, Maybe Text)])


-------------------------------------------------------------------------------
-- * Lenses for 'EventHandler'

LENS((EventHandler s),eventPred,(Event Text -> Bool))
LENS((EventHandler s),eventFunc,(Event Text -> IRC s ()))
