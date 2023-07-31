{-# LANGUAGE DuplicateRecordFields #-}
-- |
--  Module      : Cfg
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.1.0
--
-- This package provides an api for representing configuration as a haskell
-- type. This entails three general considerations: a simplified
-- representation of our haskell type so that it maps better to existing
-- configuration formats, an adapter to translate between the simplified
-- representation and a concrete configuration "source" (i.e. environment
-- variables, yaml files, etc.), and a parser that can recover the structure of
-- the haskell type from the simplified representation.
--
-- While this package provides a default source (environment variables), the
-- intention is that other packages will provide additional sources.
module Cfg
  ( -- * Concepts
    -- |
    --
    -- The core concepts in this package are:
    --
    --    * __A simplified type representation:__ The type chosen to represent our
    --    underlying haskell type is 'KeyTree.KeyTree'. This reflects the
    --    potentially nested structure of configuration, and makes it easy
    --    simply append values as leaf nodes.
    --
    --    * __Sources:__ These represent a way to build a simplified representation
    --    from as Haskell type. Source may seem like an odd name, but other
    --    names like \"Rep\", or \"Representation\" are taken and overloaded.
    --    The tree structures created by the typeclasses in "Cfg.Source" are
    --    used to request values from a configuration source.
    --
    --    * __Parsers:__ Once a request for configuration values has been made to a
    --    source, and the actual values are appended as leaf nodes on the tree
    --    representation we require a parser to pull that information out and
    --    construct a Haskell type. The parser traverses the tree and makes sure
    --    that it structurally matches our Haskell type, and then it will parse the
    --    'Data.Text.Text' values at the leaves into actual Haskell types.
    --    The api that corresponds to this can be found in "Cfg.Parser".
    --
    --    * __Deriving:__ It is a design principle of this library that the
    --    vast majority (if not all) functionality should be derivable. For
    --    this we use "GHC.Generics", and [deriving
    --    via](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/deriving_via.html).
    --    You can always hand write instances for custom functionality, but
    --    there are also a handful of options that can be specified using the
    --    deriving machinery. Documentation on those options can be found in
    --    "Cfg.Deriving".

    -- * Quickstart guide
    -- |
    --
    -- Here we will introduce some sample code that should get you up and running
    -- quickly. We will also explain some of the internals so you can see how
    -- things are wired together

    -- ** Initial configuration
    -- |
    --
    -- Let's start out with a couple types that represent some imaginary
    -- configuration for an imaginary application. This is the most basic
    -- kind of configuration we can have.
    --
    -- You will probably notice that records derive their instances via a
    -- `Config` newtype, while base types (types that represent the actual
    -- configuration values) are derived via a `Value` newtype. Values first
    -- derive a `ValueParser` instance, and then that makes it possible to
    -- derive a `ConfigParser` instance (no deriving via machinery necessary
    -- for that instance).
    --
    -- @
    -- {\-# LANGUAGE DeriveGeneric #-\}
    -- {\-# LANGUAGE DerivingVia #-\}
    --
    -- import "Cfg.Deriving.Config"
    -- import "Cfg.Deriving.Value"
    -- import "Cfg.Parser"
    -- import "Cfg.Source"
    -- import "Cfg.Source.Default"
    -- import Data.ByteString (ByteString)
    -- import GHC.Generics
    --
    -- data Environment = Development | Production
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ValueParser') via ('Value' Environment)
    --   -- Note: This is derivable via ConigParser's default instance, because we provided a ValueParser instance
    --   deriving ('ConfigParser') 
    --
    -- data WarpConfig = WarpConfig
    --   { warpConfigPort :: Int
    --   , warpConfigTimeout :: Int
    --   , warpConfigHTTP2Enabled :: Bool
    --   , warpConfigServerName :: ByteString
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('Config' WarpConfig)
    --
    -- data RedisConfig = RedisConfig
    --   { redisConfigHost :: Text
    --   , redisConfigPort :: Int
    --   , redisConfigConnectAuth :: Maybe ByteString
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('Config' RedisConfig)
    --
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('Config' AppConfig)
    -- @
    --
    -- And here is the result of generating the keys for this configuration setup
    --
    -- >>> import Cfg
    -- >>> import Text.Pretty.Simple
    -- >>> import Cfg.Env.Keys
    -- >>> pPrint $ showEnvKeys @AppConfig "_"
    -- [ "appConfigEnvironment"
    -- , "appConfigRedisSettings_redisConfigConnectAuth"
    -- , "appConfigRedisSettings_redisConfigHost"
    -- , "appConfigRedisSettings_redisConfigPort"
    -- , "appConfigWarpSettings_warpConfigHTTP2Enabled"
    -- , "appConfigWarpSettings_warpConfigPort"
    -- , "appConfigWarpSettings_warpConfigServerName"
    -- , "appConfigWarpSettings_warpConfigTimeout"
    -- ]
    
    -- ** Basic key modifiers
    -- |
    --
    -- This is okay, but there are some changes we may want to make. We will go
    -- through a series of tweaks to the example above to format the keys. We
    -- format the keys by providing formatting options through a new newtype
    -- 'ConfigOpts' that accepts a type parameter.
    --
    -- @
    -- data WarpConfig = ...
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('ConfigOpts' 'ToUpper' WarpConfig)
    --
    -- data RedisConfig = ...
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('ConfigOpts' 'ToUpper' WarpConfig)
    --
    -- data AppConfig = ...
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('ConfigOpts' 'ToUpper' AppConfig)
    -- @
    --
    -- Let's print the keys out again (note, we have to add a numbered suffix
    -- to the constructor so we don't get namespace collisions in the doc
    -- tests)
    --
    -- >>> import Cfg
    -- >>> import Text.Pretty.Simple
    -- >>> import Cfg.Env.Keys
    -- >>> pPrint $ showEnvKeys @AppConfig2 "_"
    -- [ "APPCONFIGENVIRONMENT"
    -- , "APPCONFIGREDISSETTINGS_REDISCONFIGCONNECTAUTH"
    -- , "APPCONFIGREDISSETTINGS_REDISCONFIGHOST"
    -- , "APPCONFIGREDISSETTINGS_REDISCONFIGPORT"
    -- , "APPCONFIGWARPSETTINGS_WARPCONFIGHTTP2ENABLED"
    -- , "APPCONFIGWARPSETTINGS_WARPCONFIGPORT"
    -- , "APPCONFIGWARPSETTINGS_WARPCONFIGSERVERNAME"
    -- , "APPCONFIGWARPSETTINGS_WARPCONFIGTIMEOUT"
    -- ]
    
    -- ** Multiple key modifiers
    -- |
    --
    -- This is close, but we probably want to remove the record field suffixes
    -- for our configuration. We can provide more than one formatter through
    -- tuples (up to a cardinality of 4) or a type level list. These formatters
    -- apply in order from left to right.
    --
    -- @
    -- data WarpConfig = WarpConfig
    --   { warpConfigPort :: Int
    --   , warpConfigTimeout :: Int
    --   , warpConfigHTTP2Enabled :: Bool
    --   , warpConfigServerName :: ByteString
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('ConfigOpts' ('StripPrefix' "warpConfig", 'ToUpper') WarpConfig)
    --
    -- data RedisConfig = RedisConfig
    --   { redisConfigHost :: Text
    --   , redisConfigPort :: Int
    --   , redisConfigConnectAuth :: Maybe ByteString
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') via ('ConfigOpts' ['StripPrefix' "redisConfig", 'ToUpper'] WarpConfig)
    --
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    --   deriving (Generic, Show, 'DefaultSource')
    --   deriving ('ConfigSource', 'ConfigParser') 
    --    via ('ConfigOpts' ['StripPrefix' "appConfig", 'StripSuffix' \"Settings\", ToUpper] AppConfig)
    -- @
    --
    -- >>> import Cfg
    -- >>> import Text.Pretty.Simple
    -- >>> import Cfg.Env.Keys
    -- >>> pPrint $ showEnvKeys @AppConfig3 "_"
    -- [ "ENVIRONMENT"
    -- , "REDIS_CONNECTAUTH"
    -- , "REDIS_HOST"
    -- , "REDIS_PORT"
    -- , "WARP_HTTP2ENABLED"
    -- , "WARP_PORT"
    -- , "WARP_SERVERNAME"
    -- , "WARP_TIMEOUT"
    -- ]
    
    -- ** Root key
    -- |
    --
    -- This is much better, but we might even want to go a step further and
    -- namespace our config with a rootkey. We can do this by deriving via a
    -- special type on our root config record.
    --
    -- @
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    -- deriving (Generic, Show, 'DefaultSource')
    -- deriving ('ConfigSource', 'ConfigParser') 
    --   via (
    --     'ConfigRoot' 
    --       (''TypeName' ['StripSuffix' "Config", 'ToUpper']) 
    --       ['StripPrefix' "appConfig", 'StripSuffix' \"Settings\", 'ToUpper'] 
    --       AppConfig
    --   )
    -- @
    --
    -- The first parameter to 'ConfigRoot' is either 'TypeName' or
    -- 'ConstructorName', this indicates which name will be used for the root
    -- key. You can then provide key formatters to manipulate that name.
    --
    -- >>> import Cfg
    -- >>> import Text.Pretty.Simple
    -- >>> import Cfg.Env.Keys
    -- >>> pPrint $ showEnvKeys @AppConfig4 "_"
    -- [ "APP_ENVIRONMENT"
    -- , "APP_REDIS_CONNECTAUTH"
    -- , "APP_REDIS_HOST"
    -- , "APP_REDIS_PORT"
    -- , "APP_WARP_HTTP2ENABLED"
    -- , "APP_WARP_PORT"
    -- , "APP_WARP_SERVERNAME"
    -- , "APP_WARP_TIMEOUT"
    -- ]
    
    -- ** Defaults
    -- |
    --
    -- The defaulting machinery is admittedly a bit crude. You must define a
    -- 'DefaultSource' instance for the record that contains the value you want
    -- to default. The reason the defaulting needs to be defined on the record
    -- is that we use the record field key to identify the defaulted value the
    -- value you want to default. The reason the defaulting needs to be defined
    -- on the record is that we use the record field key to identify the
    -- defaulted value.
    --
    -- __There are a bunch of gotchas with defaulting__:
    --
    --    - Since the type of 'defaults' is @Text -> Maybe Text@, the onus is
    --    on the implementor to make sure that this function correctly matches
    --    the record field name.
    --
    --    - The way it is currently implemented we use the 'defaults' function
    --    on the record field /before/ applying key modifiers.
    --
    --    - If there is a mismatch it will fail silently by not defaulting.
    --    This may result in an error when parsing (due to a missing value).
    --
    --    - You can only set defaults via their textual representation, so your
    --    defaults might fail to parse!
    --
    --    - If you declare a default on a field that is supposed to hold nested config this will break, and there is nothing at the type level to prevent you from making this mistake
    --
    -- Considering all of the above, it may be preferable to do some defaulting
    -- on the configuration side (i.e. make sure default environment variables
    -- are set, or provide default configuration files that can be modified).
    --
    -- If you still want to do defaulting on the haskell side here is how:
    --
    -- @
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    --   deriving (Generic, Show)
    --   deriving ('ConfigSource', 'ConfigParser') 
    --    via ('ConfigOpts' ['StripPrefix' "appConfig", 'StripSuffix' \"Settings\", ToUpper] AppConfig)
    --
    -- -- NOTE: If I provide a default for WarpConfig or RedisConfig this will break the configuration machinery
    -- -- so I only match on the field for 'Environment'
    --
    -- instance 'DefaultSource' (AppConfig a) where
    --   'defaults' "appConfigEnvironment" = Just "Development"
    --   'defaults' _ = Nothing
    -- @

    -- * Exports
    getConfigRaw
  , getConfig
  )
where

import Cfg.Parser 
import Data.Text (Text)
import KeyTree
import Cfg.Source
-- Haddock example imports
import Cfg.Deriving.Config 
import Cfg.Deriving.Value
import Cfg.Parser ()
import Cfg.Source ()
import Data.ByteString (ByteString)
import GHC.Generics
import Cfg.Source.Default
import Cfg.Deriving
import Cfg.Options

-- | @since 0.0.1.0
getConfigRaw
  :: (Monad m)
  => KeyTree Text Text
  -> (KeyTree Text Text -> m (KeyTree Text Text))
  -> (KeyTree Text Text -> Either e a)
  -> m (Either e a)
getConfigRaw keyTree source parse = parse <$> source keyTree

-- | @since 0.0.1.0
getConfig
  :: forall a m. (Monad m, ConfigSource a, ConfigParser a) => FetchSource m -> m (Either ConfigParseError a)
getConfig fetch = parseConfig @a <$> fetch (configSource @a)

-------------------------------------------------------
-- Examples for haddocks
-------------------------------------------------------
data Environment = Development | Production
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ValueParser) via (Value Environment)
  deriving (ConfigParser) -- Note: This is derivable via ConigParser's default instance, because we provided a ValueParser instance

-- Example 1

data WarpConfig = WarpConfig
  { warpConfigPort :: Int
  , warpConfigTimeout :: Int
  , warpConfigHTTP2Enabled :: Bool
  , warpConfigServerName :: ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (Config WarpConfig)

data RedisConfig = RedisConfig
  { redisConfigHost :: Text
  , redisConfigPort :: Int
  , redisConfigConnectAuth :: Maybe ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (Config RedisConfig)

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig
  , appConfigRedisSettings :: RedisConfig
  , appConfigEnvironment :: Environment
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (Config AppConfig)

-- Example 2
data WarpConfig2 = WarpConfig2
  { warpConfigPort :: Int
  , warpConfigTimeout :: Int
  , warpConfigHTTP2Enabled :: Bool
  , warpConfigServerName :: ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts ToUpper WarpConfig2)

data RedisConfig2 = RedisConfig2
  { redisConfigHost :: Text
  , redisConfigPort :: Int
  , redisConfigConnectAuth :: Maybe ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts ToUpper RedisConfig2)

data AppConfig2 = AppConfig2
  { appConfigWarpSettings :: WarpConfig2
  , appConfigRedisSettings :: RedisConfig2
  , appConfigEnvironment :: Environment
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts ToUpper AppConfig2)

-- Example 3
data WarpConfig3 = WarpConfig3
  { warpConfigPort :: Int
  , warpConfigTimeout :: Int
  , warpConfigHTTP2Enabled :: Bool
  , warpConfigServerName :: ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts (StripPrefix "warpConfig", ToUpper) WarpConfig3)

data RedisConfig3 = RedisConfig3
  { redisConfigHost :: Text
  , redisConfigPort :: Int
  , redisConfigConnectAuth :: Maybe ByteString
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) via (ConfigOpts [StripPrefix "redisConfig", ToUpper] RedisConfig3)

data AppConfig3 = AppConfig3
  { appConfigWarpSettings :: WarpConfig3
  , appConfigRedisSettings :: RedisConfig3
  , appConfigEnvironment :: Environment
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) 
    via (ConfigOpts [StripPrefix "appConfig", StripSuffix "Settings", ToUpper] AppConfig3)

-- Example 4
data AppConfig4 = AppConfig4
  { appConfigWarpSettings :: WarpConfig3
  , appConfigRedisSettings :: RedisConfig3
  , appConfigEnvironment :: Environment
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource, ConfigParser) 
    via (
      ConfigRoot 
        ('TypeName [StripSuffix "Config4", ToUpper]) 
        [StripPrefix "appConfig", StripSuffix "Settings", ToUpper] 
        AppConfig4
    )
