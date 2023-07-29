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
-- representation our haskell type so that it maps better to existing
-- configuration tools, an adapter to translate between the simplified
-- representation and a concrete configuration "source" (i.e. environment
-- variables, yaml files, etc.), and a parser that can translate between the
-- abstract representation and the concrete haskell type.
--
-- While this package provides a default source (environment variables), the
-- intention is that other packages will provide additional sources.
module Cfg
  ( -- * Concepts

  --

    -- |
    --
    -- The core concepts in this package are:
    --
    --    * __A simplified type representation:__ The type chosen to represent our
    --    underlying haskell type is 'Data.Tree.Tree'. This reflects the
    --    potentially nested structure of configuration, and makes it easy to nest
    --    keys and then simply append values as leaf nodes.
    --
    --    * __Sources:__ These represent a way to build a simplified representation
    --    from as Haskell type. Source may seem like an odd name, but other names
    --    like \"Rep\", or \"Representation\" are taken and overloaded. The tree
    --    structures created by the typeclasses in "Cfg.Source" are what is used to
    --    request values from a configuration source.
    --
    --    * __Parsers:__ Once a request for configuration values has been made to a
    --    source, and the actual values are appended as leaf nodes on the tree
    --    representation we require a parser to pull that information out and
    --    construct a Haskell type. The parser traverses the tree and makes sure
    --    that it structurally matches our Haskell type, and then it will parse the
    --    'Data.Text.Text' values at the leaves into actual actual Haskell types.
    --    The api that corresponds to this part can be found in "Cfg.Parser".
    --
    --    * __Deriving:__ It is a design principle of this library that the vast
    --    majority (if not all) functionality should be derivable. For this we use
    --    "GHC.Generics", and deriving via. You can always hand write instances for
    --    custom functionality, but there are also a handful of options that can be
    --    specified using the deriving machinery. Documentation on those options
    --    can be found in "Cfg.Deriving".
    --
    --    * __Roots and nesting__: In general there is a distinction between the
    --    "Root" type for a configuration, and then subtypes that are arbitrarily
    --    nested under the root type (or other nested configurations). This makes
    --    some parts of the Generic machinery easier, but also serves some
    --    practical purposes (i.e. subconfig keys are picked from record field
    --    names, while the root key is picked from the root type's type name).

    -- * Quickstart guide

  --

    -- |
    --
    -- Here we will introduce some sample code that should get you up and running
    -- quickly. We will also explain some of the internals so you can see how
    -- things are wired together

    -- ** Sample configuration

  --

    -- |
    --
    -- Let's start out with a couple types that represent some imaginary
    -- configuration for an imaginary application. You will probably notice that we
    -- derive the 'RootConfig' class for our top level config @AppConfig@ and we do
    -- this via the 'ConfigRoot' type. We derive the 'NestedConfig' class for all
    -- the nested product types via the 'SubConfig' type. Finally, for
    -- @Environment@, which is not really a configuration, but rather a value that
    -- can be configured, we derive 'NestedConfig' via 'ConfigValue'.
    --
    -- This is all probably a bit opaque, to understand the 'RootConfig' and
    -- 'NestedConfig' class better you can read the "Cfg.Source" module. To
    -- understand the deriving mechanisms better you can read the "Cfg.Deriving"
    -- module.
    --
    -- @
    -- {\-# LANGUAGE DeriveGeneric #-\}
    -- {\-# LANGUAGE DerivingVia #-\}
    --
    -- import "Cfg.Deriving" (ConfigValue, ConfigRoot, SubConfig)
    -- import "Cfg.Source" (RootConfig, NestedConfig)
    -- import Data.ByteString (ByteString)
    -- import Data.Text (Text)
    -- import GHC.Generics
    --
    -- data Environment = Development | Production
    --   deriving stock (Generic, Show)
    --   deriving ('NestedConfig') via ('ConfigValue' Environment)
    --
    -- data WarpConfig = WarpConfig
    --   { warpConfigPort :: Int
    --   , warpConfigTimeout :: Int
    --   , warpConfigHTTP2Enabled :: Bool
    --   , warpConfigServerName :: ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedConfig') via ('SubConfig' WarpConfig)
    --
    -- data RedisConfig = RedisConfig
    --   { redisConfigHost :: Text
    --   , redisConfigPort :: Int
    --   , redisConfigConnectAuth :: Maybe ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedConfig') via ('SubConfig' RedisConfig)
    --
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    --   deriving stock (Generic, Show)
    --   deriving ('RootConfig') via ('ConfigRoot' AppConfig)
    -- @

    -- ** Generated representation

  --

    -- |
    --
    -- Below we can see a doctest example that shows the internal \"simplified
    -- representation\" that this library uses.
    --
    -- >>> import Text.Pretty.Simple (pPrint)
    -- >>> import Cfg.Source () -- Pulls in the RootConfig instance for 'toRootConfig'
    -- >>> pPrint $ toRootConfig @AppConfig
    -- Node
    --     { rootLabel = "AppConfig"
    --     , subForest =
    --         [ Node
    --             { rootLabel = "appConfigWarpSettings"
    --             , subForest =
    --                 [ Node
    --                     { rootLabel = "warpConfigPort"
    --                     , subForest = []
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigTimeout"
    --                     , subForest = []
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigHTTP2Enabled"
    --                     , subForest = []
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigServerName"
    --                     , subForest = []
    --                     }
    --                 ]
    --             }
    --         , Node
    --             { rootLabel = "appConfigRedisSettings"
    --             , subForest =
    --                 [ Node
    --                     { rootLabel = "redisConfigHost"
    --                     , subForest = []
    --                     }
    --                 , Node
    --                     { rootLabel = "redisConfigPort"
    --                     , subForest = []
    --                     }
    --                 , Node
    --                     { rootLabel = "redisConfigConnectAuth"
    --                     , subForest = []
    --                     }
    --                 ]
    --             }
    --         , Node
    --             { rootLabel = "appConfigEnvironment"
    --             , subForest = []
    --             }
    --         ]
    --     }

    -- ** Parsing a representation

  --

    -- |
    --
    -- Below we are deriving just the parsers for our example data type from above.
    -- Just like the above example we use the 'RootConfig', 'SubConfig', and
    -- 'ConfigValue' types to derive the appropriate parsing classes. This time,
    -- however, there are 3 classes: 'RootParser' which should be placed on the top
    -- level configuration record, 'NestedParser' which should be derived for
    -- nested configuration product type, 'ValueParser' is derived for leaf level
    -- configuration values (you also need to derive 'NestedParser' for these, but
    -- there is a default method that just uses the 'ValueParser' so you can derive
    -- it without any strategy)
    --
    -- More information on the parsers can be found at "Cfg.Parser".
    --
    -- In the example below there is a term called @sample@ and this represents a
    -- tree that may have been retrieved from a source, and should be parsable
    -- given our type and derived instances.
    --
    -- @
    -- {\-# LANGUAGE DeriveGeneric #-\}
    -- {\-# LANGUAGE DerivingVia #-\}
    --
    -- import "Cfg.Deriving" (ConfigValue(..), SubConfig(..), ConfigRoot(..))
    -- import "Cfg.Parser" (RootParser(..), ConfigParseError, NestedParser, ValueParser)
    -- import Data.ByteString (ByteString)
    -- import Data.Text (Text)
    -- import GHC.Generics
    --
    -- data Environment = Development | Production
    --   deriving stock (Generic, Show)
    --   deriving ('ValueParser') via ('ConfigValue' Environment)
    --   deriving 'NestedParser'
    --
    -- data WarpConfig = WarpConfig
    --   { warpConfigPort :: Int
    --   , warpConfigTimeout :: Int
    --   , warpConfigHTTP2Enabled :: Bool
    --   , warpConfigServerName :: ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedParser') via ('SubConfig' WarpConfig)
    --
    -- data RedisConfig = RedisConfig
    --   { redisConfigHost :: Text
    --   , redisConfigPort :: Int
    --   , redisConfigConnectAuth :: Maybe ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedParser') via ('SubConfig' RedisConfig)
    --
    -- data AppConfig = AppConfig
    --   { appConfigWarpSettings :: WarpConfig
    --   , appConfigRedisSettings :: RedisConfig
    --   , appConfigEnvironment :: Environment
    --   }
    --   deriving stock (Generic, Show)
    --   deriving ('RootParser') via ('ConfigRoot' AppConfig)
    --
    -- sample :: Tree Text
    -- sample = Node
    --     { rootLabel = \"AppConfig\"
    --     , subForest =
    --         [ Node
    --             { rootLabel = "appConfigWarpSettings"
    --             , subForest =
    --                 [ Node
    --                     { rootLabel = "warpConfigPort"
    --                     , subForest = [ Node "8080" [] ]
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigTimeout"
    --                     , subForest = [ Node "30" [] ]
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigHTTP2Enabled"
    --                     , subForest = [ Node \"True\" [] ]
    --                     }
    --                 , Node
    --                     { rootLabel = "warpConfigServerName"
    --                     , subForest = [ Node \"MyServer\" [] ]
    --                     }
    --                 ]
    --             }
    --         , Node
    --             { rootLabel = "appConfigRedisSettings"
    --             , subForest =
    --                 [ Node
    --                     { rootLabel = "redisConfigHost"
    --                     , subForest = [ Node "https://localhost" [] ]
    --                     }
    --                 , Node
    --                     { rootLabel = "redisConfigPort"
    --                     , subForest = [ Node "6379" [] ]
    --                     }
    --                 , Node
    --                     { rootLabel = "redisConfigConnectAuth"
    --                     , subForest = [ Node "Just password" [] ]
    --                     }
    --                 ]
    --             }
    --         , Node
    --             { rootLabel = "appConfigEnvironment"
    --             , subForest = [ Node \"Development\" [] ]
    --             }
    --         ]
    --     }
    -- @
    --
    -- Here is a demonstration of running the parser on the sample tree structure
    -- shown above.
    --
    -- >>> import Text.Pretty.Simple (pPrint)
    -- >>> import Cfg.Parser () -- Pulls in the RootParser instance for 'parseRootConfig'
    -- >>> pPrint $ parseRootConfig @AppConfig sample
    -- Right
    --     ( AppConfig
    --         { appConfigWarpSettings = WarpConfig
    --             { warpConfigPort = 8080
    --             , warpConfigTimeout = 30
    --             , warpConfigHTTP2Enabled = True
    --             , warpConfigServerName = "MyServer"
    --             }
    --         , appConfigRedisSettings = RedisConfig
    --             { redisConfigHost = "https://localhost"
    --             , redisConfigPort = 6379
    --             , redisConfigConnectAuth = Just "password"
    --             }
    --         , appConfigEnvironment = Development
    --         }
    --     )

    -- ** Manipulating key format

  --

    -- |
    --
    -- The last thing we will go over is manipulating the way that we format
    -- configuration keys. Certain configuration sources have stylistic standards
    -- that may not be the same as Haskell. Therefore we offer some options for
    -- configuring their representation.
    --
    -- In the example below we will say that we are using environment variables as
    -- our configuration source. It is pretty standard to have env vars in
    -- SCREAMING_SNAKE_CASE, therefore we will apply a modifier that does that.
    --
    -- We will also use a convenience function from "Cfg.Env.Keys" to print out
    -- the expected shape of the keys after all the formatters have been applied.
    --
    -- @
    -- {\-# LANGUAGE DeriveGeneric #-\}
    -- {\-# LANGUAGE DerivingVia #-\}
    --
    -- import "Cfg.Deriving" (ConfigValue(..), SubConfig(..), ConfigRoot(..))
    -- import "Cfg.Parser" (RootParser(..), ConfigParseError, NestedParser, ValueParser)
    -- import "Cfg.Deriving.LabelModifier" (ToUpper)
    -- import "Cfg.Deriving.ConfigRoot" (ConfigRootOpts(..))
    -- import "Cfg.Deriving.SubConfig" (SubConfigOpts(..))
    -- import Data.Text (Text)
    -- import Data.ByteString (ByteString)
    -- import GHC.Generics
    --
    -- data Environment = Development | Production
    --   deriving stock (Generic, Show)
    --   deriving ('ValueParser') via ('ConfigValue' Environment)
    --   deriving 'NestedParser'
    --
    -- data EnvWarpConfig = EnvWarpConfig
    --   { envWarpConfigPort :: Int
    --   , envWarpConfigTimeout :: Int
    --   , envWarpConfigHTTP2Enabled :: Bool
    --   , envWarpConfigServerName :: ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedConfig') via ('SubConfigOpts' 'ToUpper' EnvWarpConfig)
    --   deriving ('NestedParser') via ('SubConfigOpts' 'ToUpper' EnvWarpConfig)
    --
    -- data EnvRedisConfig = EnvRedisConfig
    --   { envRedisConfigHost :: Text
    --   , envRedisConfigPort :: Int
    --   , envRedisConfigConnectAuth :: Maybe ByteString
    --   }
    --   deriving (Generic, Show)
    --   deriving ('NestedConfig') via ('SubConfigOpts' 'ToUpper' EnvRedisConfig)
    --   deriving ('NestedParser') via ('SubConfigOpts' 'ToUpper' EnvRedisConfig)
    --
    -- data EnvAppConfig = EnvAppConfig
    --   { envAppConfigWarpSettings :: EnvWarpConfig
    --   , envAppConfigRedisSettings :: EnvRedisConfig
    --   , envAppConfigEnvironment :: Environment
    --   }
    --   deriving stock (Generic, Show)
    --   deriving ('RootConfig') via ('ConfigRootOpts' 'ToUpper' 'ToUpper' EnvAppConfig)
    --   deriving ('RootParser') via ('ConfigRootOpts' 'ToUpper' 'ToUpper' EnvAppConfig)
    -- @
    --
    -- >>> import Cfg
    -- >>> import Text.Pretty.Simple
    -- >>> import Cfg.Env.Keys
    -- >>> pPrint $ showEnvKeys @EnvAppConfig "_"
    -- [ "ENVAPPCONFIG_ENVAPPCONFIGWARPSETTINGS_ENVWARPCONFIGPORT"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGWARPSETTINGS_ENVWARPCONFIGTIMEOUT"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGWARPSETTINGS_ENVWARPCONFIGHTTP2ENABLED"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGWARPSETTINGS_ENVWARPCONFIGSERVERNAME"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGREDISSETTINGS_ENVREDISCONFIGHOST"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGREDISSETTINGS_ENVREDISCONFIGPORT"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGREDISSETTINGS_ENVREDISCONFIGCONNECTAUTH"
    -- , "ENVAPPCONFIG_ENVAPPCONFIGENVIRONMENT"
    -- ]

    -- * Exports

  --

    getConfigRaw
  , getConfig
  )
where

import Cfg.Deriving (ConfigRoot (..), ConfigValue (..))
-- Imports for examples

import Cfg.Deriving.ConfigRoot (ConfigRootOpts (..))
import Cfg.Deriving.LabelModifier (ToUpper)
import Cfg.Deriving.SubConfig (SubConfig (..), SubConfigOpts (..))
import Cfg.Parser (ConfigParseError, NestedParser, RootParser (..), ValueParser)
import Cfg.Source (FetchSource, NestedConfig, RootConfig (..))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Tree (Tree (..))
import GHC.Generics

-- | @since 0.0.1.0
getConfigRaw
  :: (Monad m)
  => Tree Text
  -> (Tree Text -> m (Tree Text))
  -> (Tree Text -> Either e a)
  -> m (Either e a)
getConfigRaw keyTree source parser = parser <$> source keyTree

-- | @since 0.0.1.0
getConfig
  :: forall a m. (Monad m, RootConfig a, RootParser a) => FetchSource m -> m (Either ConfigParseError a)
getConfig fetch = parseRootConfig @a <$> fetch (toRootConfig @a)

-------------------------------------------------------
-- Examples for haddocks
-------------------------------------------------------
data Environment = Development | Production
  deriving stock (Generic, Show)
  deriving (NestedConfig) via ConfigValue Environment
  deriving (ValueParser) via ConfigValue Environment
  deriving (NestedParser)

data WarpConfig = WarpConfig
  { warpConfigPort :: Int
  , warpConfigTimeout :: Int
  , warpConfigHTTP2Enabled :: Bool
  , warpConfigServerName :: ByteString
  }
  deriving (Generic, Show)
  deriving (NestedConfig) via (SubConfig WarpConfig)
  deriving (NestedParser) via (SubConfig WarpConfig)

data RedisConfig = RedisConfig
  { redisConfigHost :: Text
  , redisConfigPort :: Int
  , redisConfigConnectAuth :: Maybe ByteString
  }
  deriving (Generic, Show)
  deriving (NestedConfig) via (SubConfig RedisConfig)
  deriving (NestedParser) via (SubConfig RedisConfig)

data AppConfig = AppConfig
  { appConfigWarpSettings :: WarpConfig
  , appConfigRedisSettings :: RedisConfig
  , appConfigEnvironment :: Environment
  }
  deriving stock (Generic, Show)
  deriving (RootConfig) via (ConfigRoot AppConfig)
  deriving (RootParser) via (ConfigRoot AppConfig)

sample :: Tree Text
sample =
  Node
    { rootLabel = "AppConfig"
    , subForest =
        [ Node
            { rootLabel = "appConfigWarpSettings"
            , subForest =
                [ Node
                    { rootLabel = "warpConfigPort"
                    , subForest = [Node "8080" []]
                    }
                , Node
                    { rootLabel = "warpConfigTimeout"
                    , subForest = [Node "30" []]
                    }
                , Node
                    { rootLabel = "warpConfigHTTP2Enabled"
                    , subForest = [Node "True" []]
                    }
                , Node
                    { rootLabel = "warpConfigServerName"
                    , subForest = [Node "MyServer" []]
                    }
                ]
            }
        , Node
            { rootLabel = "appConfigRedisSettings"
            , subForest =
                [ Node
                    { rootLabel = "redisConfigHost"
                    , subForest = [Node "https://localhost" []]
                    }
                , Node
                    { rootLabel = "redisConfigPort"
                    , subForest = [Node "6379" []]
                    }
                , Node
                    { rootLabel = "redisConfigConnectAuth"
                    , subForest = [Node "Just password" []]
                    }
                ]
            }
        , Node
            { rootLabel = "appConfigEnvironment"
            , subForest = [Node "Development" []]
            }
        ]
    }

data EnvWarpConfig = EnvWarpConfig
  { envWarpConfigPort :: Int
  , envWarpConfigTimeout :: Int
  , envWarpConfigHTTP2Enabled :: Bool
  , envWarpConfigServerName :: ByteString
  }
  deriving (Generic, Show)
  deriving (NestedConfig) via (SubConfigOpts ToUpper EnvWarpConfig)
  deriving (NestedParser) via (SubConfigOpts ToUpper EnvWarpConfig)

data EnvRedisConfig = EnvRedisConfig
  { envRedisConfigHost :: Text
  , envRedisConfigPort :: Int
  , envRedisConfigConnectAuth :: Maybe ByteString
  }
  deriving (Generic, Show)
  deriving (NestedConfig) via (SubConfigOpts ToUpper EnvRedisConfig)
  deriving (NestedParser) via (SubConfigOpts ToUpper EnvRedisConfig)

data EnvAppConfig = EnvAppConfig
  { envAppConfigWarpSettings :: EnvWarpConfig
  , envAppConfigRedisSettings :: EnvRedisConfig
  , envAppConfigEnvironment :: Environment
  }
  deriving stock (Generic, Show)
  deriving (RootConfig) via (ConfigRootOpts ToUpper ToUpper EnvAppConfig)
  deriving (RootParser) via (ConfigRootOpts ToUpper ToUpper EnvAppConfig)
