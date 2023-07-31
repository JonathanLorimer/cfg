module Cfg.Source where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.List.NonEmpty ( NonEmpty )
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Word ( Word8, Word16, Word32, Word64 )
import KeyTree ( KeyTree, Free(..) )
import Data.Map.Strict ( empty )
import Cfg.Deriving.Value (Value)

-- | @since 0.0.1.0
type FetchSource m = KeyTree Text Text -> m (KeyTree Text Text)

-- | @since 0.0.1.0
class ConfigSource a where
  configSource :: KeyTree Text Text

-- | @since 0.0.1.0
instance ConfigSource (Value a) where
  configSource = Free empty


-- | @since 0.0.1.0
deriving via (Value ()) instance ConfigSource ()

-- | @since 0.0.1.0
deriving via (Value Bool) instance ConfigSource Bool

-- | @since 0.0.1.0
deriving via (Value Char) instance ConfigSource Char

-- | @since 0.0.1.0
deriving via (Value TL.Text) instance ConfigSource TL.Text

-- | @since 0.0.1.0
deriving via (Value BL.ByteString) instance ConfigSource BL.ByteString

-- | @since 0.0.1.0
deriving via (Value BS.ByteString) instance ConfigSource BS.ByteString

-- | @since 0.0.1.0
deriving via (Value Text) instance ConfigSource Text

-- | @since 0.0.1.0
deriving via (Value [a]) instance ConfigSource [a]

-- | @since 0.0.1.0
deriving via (Value (NonEmpty a)) instance ConfigSource (NonEmpty a)

-- | @since 0.0.1.0
deriving via (Value (Vector a)) instance ConfigSource (Vector a)

-- | @since 0.0.1.0
deriving via (Value (Maybe a)) instance ConfigSource (Maybe a)

-- | @since 0.0.1.0
deriving via (Value Double) instance ConfigSource Double

-- | @since 0.0.1.0
deriving via (Value Float) instance ConfigSource Float

-- | @since 0.0.1.0
deriving via (Value Int) instance ConfigSource Int

-- | @since 0.0.1.0
deriving via (Value Int8) instance ConfigSource Int8

-- | @since 0.0.1.0
deriving via (Value Int16) instance ConfigSource Int16

-- | @since 0.0.1.0
deriving via (Value Int32) instance ConfigSource Int32

-- | @since 0.0.1.0
deriving via (Value Int64) instance ConfigSource Int64

-- | @since 0.0.1.0
deriving via (Value Integer) instance ConfigSource Integer

-- | @since 0.0.1.0
deriving via (Value Word) instance ConfigSource Word

-- | @since 0.0.1.0
deriving via (Value Word8) instance ConfigSource Word8

-- | @since 0.0.1.0
deriving via (Value Word16) instance ConfigSource Word16

-- | @since 0.0.1.0
deriving via (Value Word32) instance ConfigSource Word32

-- | @since 0.0.1.0
deriving via (Value Word64) instance ConfigSource Word64

-- | @since 0.0.1.0
deriving via (Value (a, b)) instance ConfigSource (a, b)
