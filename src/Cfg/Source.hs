module Cfg.Source where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Data (Proxy)
import Data.Int
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Tree (Tree)
import Data.Vector
import Data.Word

-- | @since 0.0.1.0
class RootConfig a where
    toRootConfig :: Proxy a -> Tree Text

-- | @since 0.0.1.0
class NestedConfig a where
    toNestedConfig :: Proxy a -> [Tree Text]

-- | @since 0.0.1.0
newtype ConfigValue a = ConfigValue {unConfigValue :: a}

-- | @since 0.0.1.0
instance NestedConfig (ConfigValue a) where
    toNestedConfig _ = []

-- | @since 0.0.1.0
deriving via (ConfigValue ()) instance NestedConfig ()

-- | @since 0.0.1.0
deriving via (ConfigValue Bool) instance NestedConfig Bool

-- | @since 0.0.1.0
deriving via (ConfigValue Char) instance NestedConfig Char

-- @since 0.0.1.0
deriving via (ConfigValue TL.Text) instance NestedConfig TL.Text

-- @since 0.0.1.0
deriving via (ConfigValue BL.ByteString) instance NestedConfig BL.ByteString

-- @since 0.0.1.0
deriving via (ConfigValue BS.ByteString) instance NestedConfig BS.ByteString

-- @since 0.0.1.0
deriving via (ConfigValue Text) instance NestedConfig Text

-- @since 0.0.1.0
deriving via (ConfigValue [a]) instance NestedConfig [a]

-- @since 0.0.1.0
deriving via (ConfigValue (NonEmpty a)) instance NestedConfig (NonEmpty a)

-- @since 0.0.1.0
deriving via (ConfigValue (Vector a)) instance NestedConfig (Vector a)

-- @since 0.0.1.0
deriving via (ConfigValue (Maybe a)) instance NestedConfig (Maybe a)

-- @since 0.0.1.0
deriving via (ConfigValue Double) instance NestedConfig Double

-- @since 0.0.1.0
deriving via (ConfigValue Float) instance NestedConfig Float

-- @since 0.0.1.0
deriving via (ConfigValue Int) instance NestedConfig Int

-- @since 0.0.1.0
deriving via (ConfigValue Int8) instance NestedConfig Int8

-- @since 0.0.1.0
deriving via (ConfigValue Int16) instance NestedConfig Int16

-- @since 0.0.1.0
deriving via (ConfigValue Int32) instance NestedConfig Int32

-- @since 0.0.1.0
deriving via (ConfigValue Int64) instance NestedConfig Int64

-- @since 0.0.1.0
deriving via (ConfigValue Integer) instance NestedConfig Integer

-- @since 0.0.1.0
deriving via (ConfigValue Word) instance NestedConfig Word

-- @since 0.0.1.0
deriving via (ConfigValue Word8) instance NestedConfig Word8

-- @since 0.0.1.0
deriving via (ConfigValue Word16) instance NestedConfig Word16

-- @since 0.0.1.0
deriving via (ConfigValue Word32) instance NestedConfig Word32

-- @since 0.0.1.0
deriving via (ConfigValue Word64) instance NestedConfig Word64

-- @since 0.0.1.0
deriving via (ConfigValue (a, b)) instance NestedConfig (a, b)
