{-# LANGUAGE UndecidableInstances #-}

module Cfg.Deriving.SubConfig where

import Cfg.Deriving.LabelModifier (LabelModifier (..))
import Cfg.Options (ConfigOptions (..), defaultConfigOptions)
import Cfg.Source (NestedConfig (..))
import Cfg.Source.NestedConfig
import GHC.Generics (Generic (..))
import Cfg.Parser.ConfigParser
import Cfg.Parser
import Data.Coerce

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
    toNestedConfig = defaultToNestedConfig @a defaultConfigOptions

instance (GetConfigOptions t, Generic a, GConfigForest (Rep a)) => NestedConfig (SubConfigOpts t a) where
    toNestedConfig = defaultToNestedConfig @a (getConfigOptions @t)

-- Parser
instance (Generic a, GNestedParser (Rep a)) => NestedParser (SubConfig a) where
  parseNestedConfig tree = coerce `asTypeOf` fmap SubConfig $ defaultParseNestedConfig defaultConfigOptions tree

instance (GetConfigOptions t, Generic a, GNestedParser (Rep a)) => NestedParser (SubConfigOpts t a) where
  parseNestedConfig tree = coerce `asTypeOf` fmap SubConfigOpts $ defaultParseNestedConfig (getConfigOptions @t) tree
