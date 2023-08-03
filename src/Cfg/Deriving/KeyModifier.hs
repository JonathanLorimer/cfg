-- |
--  Module      : Cfg.Deriving.KeyModifier
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module provides type level tags that can be used to configure string
-- transformations against configuration keys derived from things like record
-- fields.
module Cfg.Deriving.KeyModifier (
  -- * Key Modifiers
    KeyModifier(..)
  , Identity
  , ToLower
  , ToUpper
  , LowerFirst
  , UpperFirst
  , StripPrefix
  , StripSuffix
  , CamelTo
  , CamelToSnake
  , CamelToKebab
  -- * Helper Functions
  , mapFirst
  , camelTo 
  , camelToText
) where

import Cfg.Options (RootKey (..))
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Data (Proxy (..))
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits

-- | Identity transformation, corresponds to 'id', does not change the string.
--
-- @since 0.0.2.0
data Identity

-- | Lower cases all alphabetical characters, corresponds to 'Data.Text.toLower'.
--
-- @since 0.0.2.0
data ToLower

-- | Upper cases all alphabetical characters, corresponds to 'Data.Text.toUpper'.
--
-- @since 0.0.2.0
data ToUpper

-- | Lower cases the first character, corresponds to 'Data.Char.toLower'.
--
-- @since 0.0.2.0
data LowerFirst

-- | Upper cases the first character, corresponds to 'Data.Char.toUpper'.
--
-- @since 0.0.2.0
data UpperFirst

-- | Takes a type level string and removes that from the beginning of the text,
-- corresponds to 'Data.Text.stripPrefix'.
--
-- @since 0.0.2.0
data StripPrefix (prefix :: Symbol)

-- | Takes a type level string and removes that from the end of the text,
-- corresponds to 'Data.Text.stripSuffix.
--
-- @since 0.0.2.0
data StripSuffix (suffix :: Symbol)

-- | Takes a type level character known as the \"separator"\ and will break the
-- camel case string on its \"humps\" and then rejoin the string with the
-- separator.
--
-- @since 0.0.2.0
data CamelTo (separator :: Char)

-- | Specialized version of `CamelTo` where the separator is \"_\". Results in
-- snake cased strings.
--
-- @since 0.0.2.0
type CamelToSnake = CamelTo '_'

-- | Specialized version of `CamelTo` where the separator is \"-\". Results in
-- kebab cased strings.
--
-- @since 0.0.2.0
type CamelToKebab = CamelTo '-'

-- | This typeclass turns a type level \"tag\" into a function from @Text ->
-- Text@. In addition to the instances for the \"tags\", there are also
-- instances for type level lists and tuples up to an arity of 4.
--
-- __important__: For type level lists and tuples the modifiers are applied in
-- order from left to right.
--
-- >>> getKeyModifier @'[ToUpper, ToLower] "Hello World"
-- "hello world"
--
-- >>> getKeyModifier @(ToLower, ToUpper) "Hello World"
-- "HELLO WORLD"
--
-- >>> getKeyModifier @CamelToSnake "iLoveCFGProject"
-- "i_love_cfg_project"
--
-- @since 0.0.2.0
class KeyModifier t where
  getKeyModifier :: Text -> Text

instance (KeyModifier k) => KeyModifier ('TypeName k) where
  getKeyModifier = getKeyModifier @k

instance (KeyModifier k) => KeyModifier ('ConstructorName k) where
  getKeyModifier = getKeyModifier @k

instance KeyModifier Identity where
  getKeyModifier = id

instance KeyModifier '() where
  getKeyModifier = id

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

-- | Map over the first character of a stream of 'Data.Text.Text'
--
-- @since 0.0.2.0
mapFirst :: (Char -> Char) -> Text -> Maybe Text
mapFirst f text = T.uncons text <&> \(first, rest) -> f first `T.cons` rest

-- | Function for breaking a camel case string on its \"humps\" and re-joining
-- on a provided separator char.
--
-- @since 0.0.2.0
camelTo 
  :: Char -- ^ Separator character
  -> String -- ^ Camel cased string
  -> String
camelTo c = map toLower . go2 . go1
 where
  go1 "" = ""
  go1 (x : u : l : xs) | isUpper u && isLower l = x : c : u : l : go1 xs
  go1 (x : xs) = x : go1 xs
  go2 "" = ""
  go2 (l : u : xs) | isLower l && isUpper u = l : c : u : go2 xs
  go2 (x : xs) = x : go2 xs

-- | "Data.Text.Text" version of 'camelTo'
--
-- @since 0.0.2.0
camelToText :: Char -> Text -> Text
camelToText c = T.pack . camelTo c . T.unpack
