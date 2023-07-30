{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Cfg.Deriving.KeyModifier where

import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Data (Proxy (..))
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits
import Cfg.Options (RootKey)

data ToLower

data ToUpper

data LowerFirst

data UpperFirst

data StripPrefix (prefix :: Symbol)

data StripSuffix (suffix :: Symbol)

data CamelTo (separator :: Char)

type CamelToSnake = CamelTo '_'

type CamelToKebab = CamelTo '-'

-- TODO: Document left to right nature of lists and tuples
class KeyModifier t where
  getKeyModifier :: Text -> Text

instance KeyModifier k => KeyModifier (RootKey k) where
  getKeyModifier = getKeyModifier @k

instance KeyModifier '[] where
  getKeyModifier = id

instance (KeyModifier a, KeyModifier as) => KeyModifier (a ': as) where
  getKeyModifier = getKeyModifier @as . getKeyModifier @a

instance (KeyModifier a, KeyModifier b) => KeyModifier (a, b) where
  getKeyModifier = getKeyModifier @b . getKeyModifier @a

instance (KeyModifier a, KeyModifier b, KeyModifier c) => KeyModifier (a, b, c) where
  getKeyModifier = getKeyModifier @c . getKeyModifier @b . getKeyModifier @a

instance (KeyModifier a, KeyModifier b, KeyModifier c, KeyModifier d) => KeyModifier (a, b, c, d) where
  getKeyModifier = getKeyModifier @d . getKeyModifier @c . getKeyModifier @b . getKeyModifier @a

instance KeyModifier ToLower where
  getKeyModifier = T.toLower

instance KeyModifier ToUpper where
  getKeyModifier = T.toUpper

instance KeyModifier LowerFirst where
  getKeyModifier t = fromMaybe t $ mapFirst toLower t

instance KeyModifier UpperFirst where
  getKeyModifier t = fromMaybe t $ mapFirst toUpper t

instance (KnownSymbol prefix) => KeyModifier (StripPrefix prefix) where
  getKeyModifier label =
    fromMaybe label . T.stripPrefix (T.pack . symbolVal $ Proxy @prefix) $ label

instance (KnownSymbol prefix) => KeyModifier (StripSuffix prefix) where
  getKeyModifier label =
    fromMaybe label . T.stripSuffix (T.pack . symbolVal $ Proxy @prefix) $ label

instance (KnownChar separator) => KeyModifier (CamelTo separator) where
  getKeyModifier = camelToText (charVal $ Proxy @separator)

mapFirst :: (Char -> Char) -> Text -> Maybe Text
mapFirst f text = T.uncons text <&> \(first, rest) -> f first `T.cons` rest

-- |
-- 0.0.2.0
camelTo :: Char -> String -> String
camelTo c = map toLower . go2 . go1
 where
  go1 "" = ""
  go1 (x : u : l : xs) | isUpper u && isLower l = x : c : u : l : go1 xs
  go1 (x : xs) = x : go1 xs
  go2 "" = ""
  go2 (l : u : xs) | isLower l && isUpper u = l : c : u : go2 xs
  go2 (x : xs) = x : go2 xs

-- |
-- 0.0.2.0
camelToText :: Char -> Text -> Text
camelToText c = T.pack . camelTo c . T.unpack
