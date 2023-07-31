{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module Cfg.Deriving.Config where

import Cfg.Deriving.Assert (AssertTopLevelRecord)
import Cfg.Options 
import Cfg.Parser 
import Cfg.Parser.Config
import Cfg.Source 
import Cfg.Source.Config 
import Data.Coerce
import GHC.Generics
import Cfg.Deriving.KeyModifier

newtype Config a = Config {unConfig :: a}

instance (Generic a) => Generic (Config a) where
  type Rep (Config a) = Rep a
  to = Config . to
  from (Config x) = from x

newtype ConfigOpts t a = ConfigOpts {unConfigOptions :: a}

instance (Generic a) => Generic (ConfigOpts t a) where
  type Rep (ConfigOpts t a) = Rep a
  to = ConfigOpts . to
  from (ConfigOpts x) = from x

newtype ConfigRoot rootType fieldModifier a = ConfigRoot {unConfigRoot :: a}

class (KeyModifier t) => GetConfigOptions t where
  getOptions :: KeyOptions

instance (KeyModifier t) => GetConfigOptions t where
  getOptions = KeyOptions (getKeyModifier @t)

instance (Generic a) => Generic (ConfigRoot r f a) where
  type Rep (ConfigRoot r f a) = Rep a
  to = ConfigRoot . to
  from (ConfigRoot x) = from x

class (KeyModifier r, KeyModifier f) => ConfigRootOptions r f where
  configRootOptions :: RootOptions

instance (KeyModifier (TypeName k), KeyModifier f) => ConfigRootOptions (TypeName k) f where
  configRootOptions = RootOptions (TypeName $ getKeyModifier @(TypeName k)) (getKeyModifier @f)

instance (KeyModifier (ConstructorName k), KeyModifier f) => ConfigRootOptions (ConstructorName k) f where
  configRootOptions = RootOptions (ConstructorName $ getKeyModifier @(ConstructorName k)) (getKeyModifier @f)

-- Source
instance (AssertTopLevelRecord ConfigSource a, Generic a, GConfigSource (Rep a)) => ConfigSource (Config a) where
  configSource = defaultConfigSource @a defaultConfigOptions

instance
  ( GetConfigOptions t
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , GConfigSource (Rep a)
  )
  => ConfigSource (ConfigOpts t a)
  where
  configSource = defaultConfigSource @a (Key $ getOptions @t)

instance
  ( ConfigRootOptions r f
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , GConfigSource (Rep a)
  )
  => ConfigSource (ConfigRoot r f a)
  where
  configSource = defaultConfigSource @a (Root $ configRootOptions @r @f)

-- Parser
instance
  (AssertTopLevelRecord ConfigParser a, Generic a, GConfigParser (Rep a))
  => ConfigParser (Config a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap Config $ defaultParseConfig defaultConfigOptions keyTree

instance
  ( GetConfigOptions t
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , GConfigParser (Rep a)
  )
  => ConfigParser (ConfigOpts t a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap ConfigOpts $ defaultParseConfig (Key $ getOptions @t) keyTree

instance
  ( ConfigRootOptions r f
  , AssertTopLevelRecord ConfigParser a
  , Generic a
  , GConfigParser (Rep a)
  )
  => ConfigParser (ConfigRoot r f a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap ConfigRoot $ defaultParseConfig (Root $ configRootOptions @r @f) keyTree
