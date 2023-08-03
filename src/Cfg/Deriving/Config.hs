-- |
--  Module      : Cfg.Deriving.Config
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module provides types and instances for deriving 'ConfigSource' and
-- 'ConfigParser' instances via generic machinery. These types are also how you
-- modify the key representation for your configuration.
module Cfg.Deriving.Config
  ( -- * Deriving Types
    Config (..)
  , ConfigOpts (..)
  , ConfigRoot (..)

    -- * Internal Typeclasses
  , GetConfigOptions (..)
  , ConfigRootOptions (..)
  )
where

import Cfg.Deriving.Assert (AssertTopLevelRecord)
import Cfg.Deriving.KeyModifier
import Cfg.Options
import Cfg.Parser
import Cfg.Parser.Config
import Cfg.Source
import Cfg.Source.Config
import Cfg.Source.Default
import Data.Coerce
import GHC.Generics

-- $setup
-- >>> import GHC.Generics (Generic (..))
-- >>> import Cfg.Source (ConfigSource(..))
-- >>> import Cfg.Parser (ConfigParser(..))
-- >>> import Text.Pretty.Simple

-- | This newtype is the simplest deriving option. It doesn't allow you to
-- alter key names with a 'Cfg.Deriving.KeyModifier.KeyModifier', it only
-- specifies record fields as keys within the configuration tree hierarchy.
-- Therefore it is not possible to derive this for configuration /values/ (such
-- as product types without named record fields, or sum types), only top level
-- records.
--
-- ===== __Example__
--
-- >>> import GHC.Generics (Generic (..))
-- >>> import Cfg.Source (ConfigSource(..))
-- >>> import Cfg.Parser (ConfigParser(..))
-- >>> import Cfg.Deriving.Config (Config(..))
-- >>> import Cfg.Source.Default (DefaultSource(..))
-- >>> :{
-- data AppConfig = AppConfig
--   { appConfigSetting1 :: Int
--   , appConfigSetting2 :: Bool
--   , appConfigSetting3 :: String
--   }
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser) via (Config AppConfig)
-- :}
--
-- >>> pPrint $ configSource @AppConfig
-- Free
--     ( fromList
--         [
--             ( "appConfigSetting1"
--             , Free
--                 ( fromList [] )
--             )
--         ,
--             ( "appConfigSetting2"
--             , Free
--                 ( fromList [] )
--             )
--         ,
--             ( "appConfigSetting3"
--             , Free
--                 ( fromList [] )
--             )
--         ]
--     )
--
-- @since 0.0.2.0
newtype Config a = Config {unConfig :: a}

-- | @since 0.0.2.0
instance (Generic a) => Generic (Config a) where
  type Rep (Config a) = Rep a
  to = Config . to
  from (Config x) = from x

-- | This newtype is identical to 'Config' except that it accepts a type
-- argument which can be used to apply a 'Cfg.Deriving.KeyModifier.KeyModifier'
-- to each record field name when generating keys.
--
-- ===== __Example__
--
-- >>> import GHC.Generics (Generic (..))
-- >>> import Cfg.Source (ConfigSource(..))
-- >>> import Cfg.Parser (ConfigParser(..))
-- >>> import Cfg.Deriving.Config (Config(..))
-- >>> import Cfg.Source.Default (DefaultSource(..))
-- >>> :{
-- data AppConfig = AppConfig
--   { appConfigSetting1 :: Int
--   , appConfigSetting2 :: Bool
--   , appConfigSetting3 :: String
--   }
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser)
--      via (ConfigOpts '[StripPrefix "app", CamelToSnake, ToUpper] AppConfig)
-- :}
--
-- >>> pPrint $ configSource @AppConfig
-- Free
--     ( fromList
--         [
--             ( "CONFIG_SETTING1"
--             , Free
--                 ( fromList [] )
--             )
--         ,
--             ( "CONFIG_SETTING2"
--             , Free
--                 ( fromList [] )
--             )
--         ,
--             ( "CONFIG_SETTING3"
--             , Free
--                 ( fromList [] )
--             )
--         ]
--     )
--
-- @since 0.0.2.0
newtype ConfigOpts fieldModifier a = ConfigOpts {unConfigOptions :: a}

-- | @since 0.0.2.0
instance (Generic a) => Generic (ConfigOpts t a) where
  type Rep (ConfigOpts t a) = Rep a
  to = ConfigOpts . to
  from (ConfigOpts x) = from x

-- | This newtype is used to derive instances for your root configuration type
-- (i.e. the top level record for all your configuration). The only additional
-- functionality that it provides is that it lets you specify a root key, which
-- is derived from either the type name or the data constructor name. You
-- choose which name you select by providing either
-- 'Cfg.Options.ConstructorName` or 'Cfg.Options.TypeName' as the first type
-- argument to `ConfigRoot`. These `Cfg.Options.RootKey` types also take a type
-- level argument where you can provide key modifiers, if you don't want to
-- apply any key modifiers you can pass in 'Cfg.Deriving.KeyModifier.Identity'
-- or an empty tuple or an empty type level list.
--
-- ===== __@TypeName@ Example__
--
-- >>> import GHC.Generics (Generic (..))
-- >>> import Cfg.Source (ConfigSource(..))
-- >>> import Cfg.Parser (ConfigParser(..))
-- >>> import Cfg.Deriving.Config (Config(..))
-- >>> import Cfg.Source.Default (DefaultSource(..))
-- >>> import Cfg.Deriving.KeyModifier
-- >>> :{
-- data TypeNameConfig = ConfigConstructor
--   { appConfigSetting1 :: Int
--   , appConfigSetting2 :: Bool
--   , appConfigSetting3 :: String
--   }
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser)
--      via ConfigRoot
--        ('TypeName '[StripSuffix "Config", CamelToSnake, ToUpper])
--        '[StripPrefix "app", CamelToSnake, ToUpper]
--        TypeNameConfig
-- :}
--
-- >>> pPrint $ configSource @TypeNameConfig
-- Free
--     ( fromList
--         [
--             ( "TYPE_NAME"
--             , Free
--                 ( fromList
--                     [
--                         ( "CONFIG_SETTING1"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ,
--                         ( "CONFIG_SETTING2"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ,
--                         ( "CONFIG_SETTING3"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ]
--                 )
--             )
--         ]
--     )
--
-- ===== __@ConstructorName@ Example__
--
-- >>> :{
-- data TypeNameConfig = ConfigConstructor
--   { appConfigSetting1 :: Int
--   , appConfigSetting2 :: Bool
--   , appConfigSetting3 :: String
--   }
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser)
--      via ConfigRoot
--        ('ConstructorName Identity)
--        '[StripPrefix "app", CamelToSnake, ToUpper]
--        TypeNameConfig
-- :}
--
-- >>> pPrint $ configSource @TypeNameConfig
-- Free
--     ( fromList
--         [
--             ( "ConfigConstructor"
--             , Free
--                 ( fromList
--                     [
--                         ( "CONFIG_SETTING1"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ,
--                         ( "CONFIG_SETTING2"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ,
--                         ( "CONFIG_SETTING3"
--                         , Free
--                             ( fromList [] )
--                         )
--                     ]
--                 )
--             )
--         ]
--     )
--
-- @since 0.0.2.0
newtype ConfigRoot rootType fieldModifier a = ConfigRoot {unConfigRoot :: a}

-- | Typeclass for reifying type level field label modifiers into 'Cfg.Options.KeyOptions'
--
-- @since 0.0.2.0
class (KeyModifier t) => GetConfigOptions t where
  getOptions :: KeyOptions

-- | @since 0.0.2.0
instance (KeyModifier t) => GetConfigOptions t where
  getOptions = KeyOptions (getKeyModifier @t)

-- | @since 0.0.2.0
instance (Generic a) => Generic (ConfigRoot r f a) where
  type Rep (ConfigRoot r f a) = Rep a
  to = ConfigRoot . to
  from (ConfigRoot x) = from x

-- | Typeclass for reifying type level arguments into 'Cfg.Options.RootOptions'
--
-- @since 0.0.2.0
class (KeyModifier r, KeyModifier f) => ConfigRootOptions r f where
  configRootOptions :: RootOptions

-- | @since 0.0.2.0
instance (KeyModifier (TypeName k), KeyModifier f) => ConfigRootOptions (TypeName k) f where
  configRootOptions = RootOptions (TypeName $ getKeyModifier @(TypeName k)) (getKeyModifier @f)

-- | @since 0.0.2.0
instance (KeyModifier (ConstructorName k), KeyModifier f) => ConfigRootOptions (ConstructorName k) f where
  configRootOptions = RootOptions (ConstructorName $ getKeyModifier @(ConstructorName k)) (getKeyModifier @f)

-- Source

-- | @since 0.0.2.0
instance
  (AssertTopLevelRecord ConfigSource a, DefaultSource a, Generic a, GConfigSource (Rep a))
  => ConfigSource (Config a)
  where
  configSource = defaultConfigSource @a defaultConfigOptions

-- | @since 0.0.2.0
instance
  ( GetConfigOptions t
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , DefaultSource a
  , GConfigSource (Rep a)
  )
  => ConfigSource (ConfigOpts t a)
  where
  configSource = defaultConfigSource @a (Key $ getOptions @t)

-- | @since 0.0.2.0
instance
  ( ConfigRootOptions r f
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , DefaultSource a
  , GConfigSource (Rep a)
  )
  => ConfigSource (ConfigRoot r f a)
  where
  configSource = defaultConfigSource @a (Root $ configRootOptions @r @f)

-- Parser

-- | @since 0.0.2.0
instance
  (AssertTopLevelRecord ConfigParser a, Generic a, GConfigParser (Rep a))
  => ConfigParser (Config a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap Config $ defaultParseConfig defaultConfigOptions keyTree

-- | @since 0.0.2.0
instance
  ( GetConfigOptions t
  , AssertTopLevelRecord ConfigSource a
  , Generic a
  , GConfigParser (Rep a)
  )
  => ConfigParser (ConfigOpts t a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap ConfigOpts $ defaultParseConfig (Key $ getOptions @t) keyTree

-- | @since 0.0.2.0
instance
  ( ConfigRootOptions r f
  , AssertTopLevelRecord ConfigParser a
  , Generic a
  , GConfigParser (Rep a)
  )
  => ConfigParser (ConfigRoot r f a)
  where
  parseConfig keyTree = coerce `asTypeOf` fmap ConfigRoot $ defaultParseConfig (Root $ configRootOptions @r @f) keyTree
