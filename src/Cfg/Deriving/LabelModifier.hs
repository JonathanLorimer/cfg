{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Cfg.Deriving.LabelModifier where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (fromMaybe)
import Data.Data (Proxy(..))
import Data.Char (toLower, isUpper, isLower)
import GHC.TypeLits

data ToLower

data ToUpper

data StripPrefix (prefix :: Symbol)

data StripSuffix (suffix :: Symbol)

data CamelTo (separator :: Char)

type CamelToSnake = CamelTo '_'

type CamelToKebab = CamelTo '-'

-- TODO: Document left to right nature of lists and tuples
class LabelModifier t where
    getLabelModifier :: Text -> Text

instance LabelModifier '[] where
  getLabelModifier = id

instance (LabelModifier a, LabelModifier as) => LabelModifier (a ': as) where
  getLabelModifier = getLabelModifier @as . getLabelModifier @a

instance (LabelModifier a, LabelModifier b) => LabelModifier (a, b) where
  getLabelModifier = getLabelModifier @b . getLabelModifier @a

instance (LabelModifier a, LabelModifier b, LabelModifier c) => LabelModifier (a, b, c) where
  getLabelModifier = getLabelModifier @c . getLabelModifier @b . getLabelModifier @a

instance (LabelModifier a, LabelModifier b, LabelModifier c, LabelModifier d) => LabelModifier (a, b, c, d) where
  getLabelModifier = getLabelModifier @d . getLabelModifier @c . getLabelModifier @b . getLabelModifier @a

instance LabelModifier ToLower where
    getLabelModifier = T.toLower

instance LabelModifier ToUpper where
    getLabelModifier = T.toUpper

instance KnownSymbol prefix => LabelModifier (StripPrefix prefix) where
  getLabelModifier label = 
    fromMaybe label . T.stripPrefix (T.pack . symbolVal $ Proxy @prefix) $ label

instance KnownSymbol prefix => LabelModifier (StripSuffix prefix) where
  getLabelModifier label = 
    fromMaybe label . T.stripSuffix (T.pack . symbolVal $ Proxy @prefix) $ label

instance (KnownChar separator) => LabelModifier (CamelTo separator) where
  getLabelModifier = camelToText (charVal $ Proxy @separator) 

-- |
-- 0.0.2.0
camelTo :: Char -> String -> String
camelTo c = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs

-- |
-- 0.0.2.0
camelToText :: Char -> Text -> Text
camelToText c = T.pack . camelTo c . T.unpack
