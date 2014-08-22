{-# LANGUAGE OverloadedStrings #-}

-- |Functions for connecting to, and dealing with, network sockets
module Network.IRC.IDTE.Net
    ( connect
    , connectWithTLS
    , connectWithTLS'
    , defaultCiphers
    , withTLS
    , whenTLS
    , send
    , recv
    , disconnect
    ) where

import Control.Applicative    ((<$>))
import Control.Monad          (liftM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Random.AESCtr   (makeSystem)
import Data.ByteString        (singleton)
import Data.ByteString.Lazy   (fromChunks)
import Data.Default           (def)
import Data.Monoid            ((<>))
import Data.X509.Validation   (FailedReason(..))
import Network                (HostName)
import Network.IRC            (Message, encode, decode)
import Network.IRC.IDTE.Types
import Network.Socket         (AddrInfo(..), Family(..), SockAddr(..), Socket, SocketType(..), defaultHints, defaultProtocol, getAddrInfo, sClose, socket)
import Network.TLS
import Network.TLS.Extra      (ciphersuite_all)
import System.IO.Error        (catchIOError)

import qualified Network.Socket            as S
import qualified Network.Socket.ByteString as SB

-- *Initialisation

-- |Connect to a host and port, if possible, otherwise return the
-- error text.
connect :: MonadIO m => HostName -> Int -> m (Either String Socket)
connect host port = risky riskyConnect

  where riskyConnect = do
          sock <- socket AF_INET Stream defaultProtocol

          -- Attempt connection
          addr <- getAddrInfo (Just defaultHints) (Just host) Nothing
          case addr of
            (a:_) -> case addrAddress a of
                      SockAddrInet _ hostaddr -> S.connect sock $ SockAddrInet (fromIntegral port) hostaddr
                      _ -> ioError $ userError "Failed to connect to host."
            _ -> ioError $ userError "Could not resolve hostname."

          return sock


-- |Like `connect`, but also try to negotiate a TLS connection.
connectWithTLS :: MonadIO m => HostName -> Int -> m (Either String (Socket, Context))
connectWithTLS host port = connectWithTLS' host port defaultCiphers

-- |Like `connectWithTLS`, but using custom ciphers.
connectWithTLS' :: MonadIO m => HostName -> Int -> [Cipher] -> m (Either String (Socket, Context))
connectWithTLS' host port ciphers = do
  -- Get an unencrypted connection
  plain <- connect host port

  case plain of
    Right sock -> risky $ do
      -- Try only the requested ciphers.
      let supported = def { supportedCiphers = ciphers }

      -- Accept self-signed certificates and certificates from unknown
      -- CAs
      let acceptable = [SelfSigned, UnknownCA]
      let hooks = def { onServerCertificate = \cs vc sid cc -> do
                          reasons <- onServerCertificate def cs vc sid cc
                          return $ filter (flip notElem acceptable) reasons
                      }

      -- The bytestring ("deadbeef") given is used to differentiate
      -- services on the same host which may have differing
      -- certificates. As it's a reasonable assumption that IRC
      -- servers don't have other TLS-using services on the same host,
      -- the choice is not important.
      let clientctx = (defaultParamsClient host "deadbeef") { clientHooks     = hooks
                                                            , clientSupported = supported }

      -- Perform the TLS handshake
      rng <- makeSystem
      ctx <- contextNew sock clientctx rng
      handshake ctx

      return (sock, ctx)

    Left err -> return $ Left err

-- |Default allowable ciphers, ordered from strong to weak.
defaultCiphers :: [Cipher]
defaultCiphers = ciphersuite_all

-- *Combinators

-- |Run one of two functions depending on whether the connection is
-- encrypted or not.
withTLS :: (Context -> IRC a) -> (Socket -> IRC a) -> IRC a
withTLS tlsf plainf = do
  tls <- _tls    <$> connectionConfig
  s   <- _socket <$> connectionConfig

  case tls of
    Just ctx -> tlsf ctx
    Nothing  -> plainf s

-- |Run the provided function when there is a TLS context.
whenTLS :: (Context -> IRC ()) -> IRC ()
whenTLS tlsf = withTLS tlsf (const $ return ())

-- *Messaging

-- |Send a message.
send :: Message -> IRC ()
send msg = withTLS (\ctx -> sendData ctx $ fromChunks [msg'])
                   (\sck -> liftIO . SB.sendAll sck $ msg')
    where msg' = encode msg <> "\r\n"

-- |Receive a message.
recv :: IRC (Maybe Message)
recv = decode <$> withTLS recvData recvLine

    where recvLine s = liftIO $ go s "" Nothing

          go s bs Nothing   = getOne s >>= go s bs . Just
          go s bs (Just w8) = do
            w8' <- getOne s
            if w8 == cr && w8' == lf
            then return bs
            else go s (bs <> w8) $ Just w8'

          getOne s = SB.recv s 1

          cr = singleton 0o015
          lf = singleton 0o012

-- *Termination

-- |Close the socket, tearing down the TLS session if there is one.
disconnect :: IRC ()
disconnect = do
  whenTLS bye
  s <- _socket <$> connectionConfig
  liftIO $ sClose s

-- *Miscellaneous

-- |Try something risky, catching exceptions and turning them to
-- strings on error.
risky :: MonadIO m => IO a -> m (Either String a)
risky x = liftIO $ risk `catchIOError` exc
    where risk = liftM Right x
          exc  = return . Left . show
