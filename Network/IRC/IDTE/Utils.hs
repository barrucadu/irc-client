-- |Internal utility functions.
module Network.IRC.IDTE.Utils where

import Data.ByteString    (ByteString)
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- |Apply a function after first interpreting its argument as UTF-8.
(<$) :: (Text -> a) -> ByteString -> a
f <$ x = f $ decodeUtf8 x

infixl 8 <$

-- |Apply a functorial function after first interpreting its argument
-- as containing UTF-8.
(<$:) :: Functor f => (f Text -> a) -> f ByteString -> a
f <$: x = f $ fmap decodeUtf8 x

infixl 8 <$:

-- |Apply a function after first rendering its argument to UTF-8 bytes.
(>$) :: (ByteString -> a) -> Text -> a
f >$ x = f $ encodeUtf8 x

infixl 8 >$

-- |Apply a functorial function after first rendering its contents to
-- UTF-8 bytes.
(>$:) :: Functor f => (f ByteString -> a) -> f Text -> a
f >$: x = f $ fmap encodeUtf8 x

infixl 8 >$:
