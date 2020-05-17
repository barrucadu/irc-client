-- |
-- Module      : Network.IRC.Client.Lens
-- Copyright   : (c) 2017 Michael Walker
-- License     : MIT
-- Maintainer  : Michael Walker <mike@barrucadu.co.uk>
-- Stability   : experimental
-- Portability : CPP
--
-- 'Lens'es and 'Prism's.
module Network.IRC.Client.Lens where

import           Control.Concurrent.STM            (TVar)
import           Control.Monad.Catch               (SomeException)
import           Data.ByteString                   (ByteString)
import           Data.Profunctor                   (Choice(right'),
                                                    Profunctor(dimap))
import           Data.Text                         (Text)
import           Data.Time                         (NominalDiffTime)

import           Network.IRC.Client.Internal.Lens
import           Network.IRC.Client.Internal.Types

{-# ANN module ("HLint: ignore Redundant lambda") #-}

-- CPP seem to dislike the first ' on the RHS…
-- This style of CPP usage doesn't work with clang, which means won't work on Mac.
{-
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

#define PRISM(S,C,ARG,TUP,A) \
    {-| PRIME()Prism' for 'C'. -}; \
    {-# INLINE _/**/C #-}; \
    _/**/C :: Prism' S A; \
    _/**/C = dimap (\ s -> case s of C ARG -> Right TUP; _ -> Left s) \
        (either pure $ fmap (\ TUP -> C ARG)) . right'

-}

-------------------------------------------------------------------------------
-- * Lenses for 'IRCState'

{-# INLINE connectionConfig #-}
{-| 'Getter' for '_connectionConfig'. -}
connectionConfig :: Getter (IRCState s) (ConnectionConfig s)
connectionConfig = \ afb s -> (\ b -> s {_connectionConfig = b}) <$> afb (_connectionConfig s)

{-# INLINE userState #-}
{-| 'Lens' for '_userState'. -}
userState :: Lens' (IRCState s) (TVar s)
userState = \ afb s -> (\ b -> s {_userState = b}) <$> afb (_userState s)

{-# INLINE instanceConfig #-}
{-| 'Lens' for '_instanceConfig'. -}
instanceConfig :: Lens' (IRCState s) (TVar (InstanceConfig s))
instanceConfig = \ afb s -> (\ b -> s {_instanceConfig = b}) <$> afb (_instanceConfig s)

{-# INLINE connectionState #-}
{-| 'Lens' for '_connectionState'. -}
connectionState :: Lens' (IRCState s) (TVar ConnectionState)
connectionState = \ afb s -> (\ b -> s {_connectionState = b}) <$> afb (_connectionState s)

-------------------------------------------------------------------------------
-- * Lenses for 'ConnectionConfig'

{-# INLINE server #-}
{-| 'Getter' for '_server'. -}
server :: Getter (ConnectionConfig s) ByteString
server = \ afb s -> (\ b -> s {_server = b}) <$> afb (_server s)

{-# INLINE port #-}
{-| 'Getter' for '_port'. -}
port :: Getter (ConnectionConfig s) Int
port = \ afb s -> (\ b -> s {_port = b}) <$> afb (_port s)

{-# INLINE username #-}
{-| 'Lens' for '_username'. -}
username :: Lens' (ConnectionConfig s) Text
username = \ afb s -> (\ b -> s {_username = b}) <$> afb (_username s)

{-# INLINE realname #-}
{-| 'Lens' for '_realname'. -}
realname :: Lens' (ConnectionConfig s) Text
realname = \ afb s -> (\ b -> s {_realname = b}) <$> afb (_realname s)

{-# INLINE password #-}
{-| 'Lens' for '_password'. -}
password :: Lens' (ConnectionConfig s) (Maybe Text)
password = \ afb s -> (\ b -> s {_password = b}) <$> afb (_password s)

{-# INLINE flood #-}
{-| 'Lens' for '_flood'. -}
flood :: Lens' (ConnectionConfig s) NominalDiffTime
flood = \ afb s -> (\ b -> s {_flood = b}) <$> afb (_flood s)

{-# INLINE timeout #-}
{-| 'Lens' for '_timeout'. -}
timeout :: Lens' (ConnectionConfig s) NominalDiffTime
timeout = \ afb s -> (\ b -> s {_timeout = b}) <$> afb (_timeout s)

{-# INLINE onconnect #-}
{-| 'Lens' for '_onconnect'. -}
onconnect :: Lens' (ConnectionConfig s) (IRC s ())
onconnect = \ afb s -> (\ b -> s {_onconnect = b}) <$> afb (_onconnect s)

{-# INLINE ondisconnect #-}
{-| 'Lens' for '_ondisconnect'. -}
ondisconnect :: Lens' (ConnectionConfig s) (Maybe SomeException -> IRC s ())
ondisconnect = \ afb s -> (\ b -> s {_ondisconnect = b}) <$> afb (_ondisconnect s)

{-# INLINE logfunc #-}
{-| 'Lens' for '_logfunc'. -}
logfunc :: Lens' (ConnectionConfig s) (Origin -> ByteString -> IO ())
logfunc = \ afb s -> (\ b -> s {_logfunc = b}) <$> afb (_logfunc s)

-------------------------------------------------------------------------------
-- * Lenses for 'InstanceConfig'

{-# INLINE nick #-}
{-| 'Lens' for '_nick'. -}
nick :: Lens' (InstanceConfig s) Text
nick = \ afb s -> (\ b -> s {_nick = b}) <$> afb (_nick s)

{-# INLINE channels #-}
{-| 'Lens' for '_channels'. -}
channels :: Lens' (InstanceConfig s) [Text]
channels = \ afb s -> (\ b -> s {_channels = b}) <$> afb (_channels s)

{-# INLINE version #-}
{-| 'Lens' for '_version'. -}
version :: Lens' (InstanceConfig s) Text
version = \ afb s -> (\ b -> s {_version = b}) <$> afb (_version s)

{-# INLINE handlers #-}
{-| 'Lens' for '_version'. -}
handlers :: Lens' (InstanceConfig s) [EventHandler s]
handlers = \ afb s -> (\ b -> s {_handlers = b}) <$> afb (_handlers s)

{-# INLINE ignore #-}
{-| 'Lens' for '_ignore'. -}
ignore :: Lens' (InstanceConfig s) [(Text, Maybe Text)]
ignore = \ afb s -> (\ b -> s {_ignore = b}) <$> afb (_ignore s)

-------------------------------------------------------------------------------
-- * Prisms for 'ConnectionState'

{-| 'Prism' for 'Connected'. -}
{-# INLINE _Connected #-}
_Connected :: Prism' ConnectionState ()
_Connected = dimap (\ s -> case s of Connected -> Right (); _ -> Left s)
    (either pure $ fmap (\ () -> Connected)) . right'

{-| 'Prism' for 'Disconnecting'. -}
{-# INLINE _Disconnecting #-}
_Disconnecting :: Prism' ConnectionState ()
_Disconnecting = dimap (\ s -> case s of Disconnecting -> Right (); _ -> Left s)
    (either pure $ fmap (\ () -> Disconnecting)) . right'

{-| 'Prism' for 'Disconnected'. -}
{-# INLINE _Disconnected #-}
_Disconnected :: Prism' ConnectionState ()
_Disconnected = dimap (\ s -> case s of Disconnected -> Right (); _ -> Left s)
    (either pure $ fmap (\ () -> Disconnected)) . right'

-------------------------------------------------------------------------------
-- * Prisms for 'Origin'

{-| 'Prism' for 'FromServer'. -}
{-# INLINE _FromServer #-}
_FromServer :: Prism' Origin ()
_FromServer = dimap (\ s -> case s of FromServer -> Right (); _ -> Left s)
    (either pure $ fmap (\ () -> FromServer)) . right'

{-| 'Prism' for 'FromClient'. -}
{-# INLINE _FromClient #-}
_FromClient :: Prism' Origin ()
_FromClient = dimap (\ s -> case s of FromClient -> Right (); _ -> Left s)
    (either pure $ fmap (\ () -> FromClient)) . right'
