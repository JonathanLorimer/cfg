{-# LANGUAGE UndecidableInstances #-}
module Cfg.Deriving.SubConfig where

import GHC.Generics (Generic (..))
import Cfg.Options (ConfigOptions(..), defaultConfigOptions)
import Cfg.Deriving.LabelModifier (LabelModifier(..))
import Cfg.Source.NestedConfig
import Cfg.Source (NestedConfig (..))
import Data.Data (Proxy(..))

newtype SubConfig a = SubConfig {unSubConfig :: a}

instance Generic a => Generic (SubConfig a) where
  type Rep (SubConfig a) = Rep a
  to = SubConfig . to
  from (SubConfig x) = from x

newtype SubConfigOpts t a = SubConfigOpts {unSubConfigOpts :: a}

instance Generic a => Generic (SubConfigOpts t a) where
  type Rep (SubConfigOpts t a) = Rep a
  to = SubConfigOpts . to
  from (SubConfigOpts x) = from x

class GetConfigOptions t where
  getConfigOptions :: ConfigOptions

instance (LabelModifier t) => GetConfigOptions t where
  getConfigOptions = ConfigOptions (getLabelModifier @t)

-- Source
instance (Generic a, GConfigForest (Rep a)) => NestedConfig (SubConfig a) where
  toNestedConfig _ = defaultToNestedConfig defaultConfigOptions (Proxy @a)

instance (GetConfigOptions t, Generic a, GConfigForest (Rep a)) => NestedConfig (SubConfigOpts t a) where
  toNestedConfig _ = defaultToNestedConfig (getConfigOptions @t) (Proxy @a)

-- Parser
-- instance (Generic a, GConfigForest (Rep a)) => NestedConfig (SubConfig a) where
--   toNestedConfig _ = defaultToNestedConfig defaultConfigOptions (Proxy @a)
--
-- instance (GetConfigOptions t, Generic a, GConfigForest (Rep a)) => NestedConfig (SubConfigOpts t a) where
--   toNestedConfig _ = defaultToNestedConfig (getConfigOptions @t) (Proxy @a)
