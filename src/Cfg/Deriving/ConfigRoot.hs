{-# LANGUAGE UndecidableInstances #-}

module Cfg.Deriving.ConfigRoot where

import Cfg.Deriving.Assert (AssertTopLevelRecord)
import Cfg.Deriving.LabelModifier (LabelModifier (..))
import Cfg.Options (ConfigOptions (..), RootOptions (..), defaultRootOptions)
import Cfg.Source (RootConfig (..))
import Cfg.Source.RootConfig (GConfigTree, defaultToRootConfig)
import Data.Data (Proxy (..))
import GHC.Generics

newtype ConfigRoot a = ConfigRoot {unConfigRoot :: a}

instance Generic a => Generic (ConfigRoot a) where
    type Rep (ConfigRoot a) = Rep a
    to = ConfigRoot . to
    from (ConfigRoot x) = from x

newtype ConfigRootOpts t t' a = ConfigRootOpts {unConfigRootOpts :: a}

instance Generic a => Generic (ConfigRootOpts t t' a) where
    type Rep (ConfigRootOpts t t' a) = Rep a
    to = ConfigRootOpts . to
    from (ConfigRootOpts x) = from x

class (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
    getConfigRootOptions :: RootOptions

instance (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
    getConfigRootOptions = RootOptions (getLabelModifier @t) (ConfigOptions $ getLabelModifier @t')

-- Source
instance (AssertTopLevelRecord RootConfig a, Generic a, GConfigTree (Rep a)) => RootConfig (ConfigRoot a) where
    toRootConfig _ = defaultToRootConfig defaultRootOptions (Proxy @a)

instance (LabelModifier t, LabelModifier t', AssertTopLevelRecord RootConfig a, Generic a, GConfigTree (Rep a)) => RootConfig (ConfigRootOpts t t' a) where
    toRootConfig _ = defaultToRootConfig (getConfigRootOptions @t @t') (Proxy @a)

-- Parser
-- instance (AssertTopLevelRecord RootConfig a, Generic a, GConfigTree (Rep a)) => RootConfig (ConfigRoot a) where
--   toRootConfig _ = defaultToRootConfig defaultRootOptions (Proxy @a)
--
-- instance
--   ( LabelModifier t
--   , LabelModifier t'
--   , AssertTopLevelRecord RootConfig a
--   , Generic a
--   , GConfigTree (Rep a)
--   ) => RootConfig (ConfigRootOpts t t' a) where
--     toRootConfig _ = defaultToRootConfig (getConfigRootOptions @t @t') (Proxy @a)
