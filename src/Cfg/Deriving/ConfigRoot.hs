{-# LANGUAGE UndecidableInstances #-}

module Cfg.Deriving.ConfigRoot where

import Cfg.Deriving.Assert (AssertTopLevelRecord)
import Cfg.Options 
import Cfg.Parser 
import Cfg.Parser.ConfigParser
import Cfg.Source 
import Cfg.Source.RootConfig 
import Data.Coerce
import GHC.Generics

newtype Config a = Config {unConfig :: a}

instance (Generic a) => Generic (Config a) where
  type Rep (Config a) = Rep a
  to = Config . to
  from (Config x) = from x

-- newtype ConfigRootOpts t t' a = ConfigRootOpts {unConfigRootOpts :: a}
--
-- instance (Generic a) => Generic (ConfigRootOpts t t' a) where
--   type Rep (ConfigRootOpts t t' a) = Rep a
--   to = ConfigRootOpts . to
--   from (ConfigRootOpts x) = from x
--
-- class (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
--   getConfigRootOptions :: RootOptions
--
-- instance (LabelModifier t, LabelModifier t') => GetConfigRootOptions t t' where
--   getConfigRootOptions = RootOptions (getLabelModifier @t) (ConfigOptions $ getLabelModifier @t')

-- Source
instance (AssertTopLevelRecord ConfigSource a, Generic a, GConfigSource (Rep a)) => ConfigSource (Config a) where
  configSource = defaultConfigSource @a defaultConfigOptions

-- instance
--   ( LabelModifier t
--   , LabelModifier t'
--   , AssertTopLevelRecord RootConfig a
--   , Generic a
--   , GConfigTree (Rep a)
--   )
--   => RootConfig (ConfigRootOpts t t' a)
--   where
--   toRootConfig = defaultToRootConfig @a (getConfigRootOptions @t @t')

-- Parser
instance
  (AssertTopLevelRecord ConfigParser a, Generic a, GConfigParser (Rep a))
  => ConfigParser (Config a)
  where
  parseConfig tree = coerce `asTypeOf` fmap Config $ defaultParseConfig defaultConfigOptions tree

-- instance
--   ( LabelModifier t
--   , LabelModifier t'
--   , AssertTopLevelRecord RootConfig a
--   , Generic a
--   , GConfigParser (Rep a)
--   )
--   => RootParser (ConfigRootOpts t t' a)
--   where
--   parseRootConfig tree = coerce `asTypeOf` fmap ConfigRootOpts $ defaultParseRootConfig (getConfigRootOptions @t @t') tree
