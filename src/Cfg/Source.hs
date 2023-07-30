module Cfg.Source where

import Cfg.Deriving.ConfigValue (ConfigValue)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Word ( Word8, Word16, Word32, Word64 )
import KeyTree ( KeyTree, Free(Free) )
import Data.Map.Strict ( empty )

-- | @since 0.0.1.0
type FetchSource m = KeyTree Text Text -> m (KeyTree Text Text)

-- | @since 0.0.1.0
class ConfigSource a where
  configSource :: KeyTree Text Text

instance ConfigSource (ConfigValue a) where
  configSource = Free empty

-- | @since 0.0.1.0
deriving via (ConfigValue ()) instance ConfigSource ()

-- | @since 0.0.1.0
deriving via (ConfigValue Bool) instance ConfigSource Bool

-- | @since 0.0.1.0
deriving via (ConfigValue Char) instance ConfigSource Char

-- | @since 0.0.1.0
deriving via (ConfigValue TL.Text) instance ConfigSource TL.Text

-- | @since 0.0.1.0
deriving via (ConfigValue BL.ByteString) instance ConfigSource BL.ByteString

-- | @since 0.0.1.0
deriving via (ConfigValue BS.ByteString) instance ConfigSource BS.ByteString

-- | @since 0.0.1.0
deriving via (ConfigValue Text) instance ConfigSource Text

-- | @since 0.0.1.0
deriving via (ConfigValue [a]) instance ConfigSource [a]

-- | @since 0.0.1.0
deriving via (ConfigValue (NonEmpty a)) instance ConfigSource (NonEmpty a)

-- | @since 0.0.1.0
deriving via (ConfigValue (Vector a)) instance ConfigSource (Vector a)

-- | @since 0.0.1.0
deriving via (ConfigValue (Maybe a)) instance ConfigSource (Maybe a)

-- | @since 0.0.1.0
deriving via (ConfigValue Double) instance ConfigSource Double

-- | @since 0.0.1.0
deriving via (ConfigValue Float) instance ConfigSource Float

-- | @since 0.0.1.0
deriving via (ConfigValue Int) instance ConfigSource Int

-- | @since 0.0.1.0
deriving via (ConfigValue Int8) instance ConfigSource Int8

-- | @since 0.0.1.0
deriving via (ConfigValue Int16) instance ConfigSource Int16

-- | @since 0.0.1.0
deriving via (ConfigValue Int32) instance ConfigSource Int32

-- | @since 0.0.1.0
deriving via (ConfigValue Int64) instance ConfigSource Int64

-- | @since 0.0.1.0
deriving via (ConfigValue Integer) instance ConfigSource Integer

-- | @since 0.0.1.0
deriving via (ConfigValue Word) instance ConfigSource Word

-- | @since 0.0.1.0
deriving via (ConfigValue Word8) instance ConfigSource Word8

-- | @since 0.0.1.0
deriving via (ConfigValue Word16) instance ConfigSource Word16

-- | @since 0.0.1.0
deriving via (ConfigValue Word32) instance ConfigSource Word32

-- | @since 0.0.1.0
deriving via (ConfigValue Word64) instance ConfigSource Word64

-- | @since 0.0.1.0
deriving via (ConfigValue (a, b)) instance ConfigSource (a, b)
