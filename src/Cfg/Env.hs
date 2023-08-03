-- |
--  Module      : Cfg.Env
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains all the functions for interacting with environment
-- variables as a configuration source.
module Cfg.Env
  ( -- * Retrieval Functions
    envSourceSep
  , envSource
  , getEnvConfigSep
  , getEnvConfig

    -- * Printing Functions
  , printDotEnv'
  , printDotEnv
  )
where

import Cfg
import Cfg.Env.Keys
import Cfg.Parser
import Cfg.Source
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (writeFile)
import KeyTree
import System.Environment (lookupEnv)
import Prelude hiding (writeFile)

-- | This function folds the tree from root to leaf accumulating the keys along
-- the way. At the leaf we lookup the aggregated key in the environment, if
-- there is a default then we use that for missing keys.
--
-- If you are looking at the source code this is what the functions in the
-- @where@ clause do:
--
--    - @valF@: Gets called on 'Pure' values in the original tree passed in,
--    these indicate defaulted values, so we use the default if looking the
--    value up in the environment failed.
--
--    - @accF@: This operates on the accumulated key, and is responsible for
--    looking up the value in the environment when we hit the @Free M.empty@
--    case.
--
--    - @stepF@: This is the step function for the fold and accumulates the
--    keys as we traverse down the tree.
--
--    - @mkKey@: This function is the same as 'Cfg.Env.Keys.getEnvKey' except
--    that it uses @flip mappend@. The reason for this is that we insert keys
--    into the accumulator as we traverse down so they end up in reversed
--    order, then we @foldr@ over them so we just need to make sure that we are
--    placing the elements at the end of the list on the left hand side of the
--    aggregate key.
--
-- @since 0.0.1.0
envSourceSep
  :: forall m
   . (MonadIO m)
  => Text
  -- ^ Separator
  -> KeyTree Text Text
  -- ^ Configuration source
  -> m (KeyTree Text Text)
  -- ^ Configuration tree with values filled in
envSourceSep sep = mayAppendTraverse valF accF stepF []
 where
  valF :: [Text] -> Text -> m Text
  valF keys def = fromMaybe def <$> accF keys

  accF :: [Text] -> m (Maybe Text)
  accF = fmap (fmap T.pack) . liftIO . lookupEnv . T.unpack . mkKey

  stepF :: Text -> [Text] -> KeyForest Text Text -> [Text]
  stepF key acc _ = key : acc

  mkKey :: [Text] -> Text
  mkKey keys = foldr (flip mappend) "" $ intersperse sep keys

-- | This function is the same as 'envSourceSep' but with the separator specialized to \"_\".
--
-- >>> import System.Environment
-- >>> import Data.Map qualified as M
-- >>> setEnv "A_B" "Functor"
-- >>> setEnv "A_C" "Applicative"
-- >>> setEnv "A_D" "Monad"
-- >>> envSource $ Free $ M.fromList [("A", Free $ M.fromList [("B", Free M.empty), ("C", Free M.empty), ("D", Free M.empty)])]
-- Free (fromList [("A",Free (fromList [("B",Pure "Functor"),("C",Pure "Applicative"),("D",Pure "Monad")]))])
--
-- @since 0.0.1.0
envSource :: (MonadFail m, MonadIO m) => KeyTree Text Text -> m (KeyTree Text Text)
envSource = envSourceSep "_"

-- | This function can be used to print a dotenv style file with all the
-- aggregate keys, none of the values will be filled in.
--
-- Useful for testing what your expected environment variables should look
-- like, and generating an env var file template.
--
-- @since 0.0.1.0
printDotEnv'
  :: FilePath
  -- ^ Destination filepath
  -> Text
  -- ^ Separator
  -> KeyTree Text Text
  -- ^ Source representation
  -> IO ()
printDotEnv' path sep = writeFile path . foldMap (\line -> "export " <> line <> "=\n") . showEnvKeys' sep

-- | Requires a type annotation for your configuration type (with a
-- 'ConfigSource' and 'ConfigParser' instance), and a separator, and will go
-- out and fetch the values from environment variables then return your type
-- parsed from those values.
--
-- @getEnvConfigSep \@AppConfig "_"@
--
-- @since 0.0.1.0
getEnvConfigSep
  :: forall a m
   . (MonadFail m, MonadIO m, ConfigSource a, ConfigParser a)
  => Text
  -- ^ Separator
  -> m (Either ConfigParseError a)
getEnvConfigSep sep = getConfig $ envSourceSep sep

-- | The same as 'getEnvConfigSep' but with the separator hard coded to \"_\"
--
-- @since 0.0.1.0
getEnvConfig
  :: forall a m. (MonadFail m, MonadIO m, ConfigSource a, ConfigParser a) => m (Either ConfigParseError a)
getEnvConfig = getConfig $ envSource

-- | The same as 'printDotEnv'' but with the separator hard coded to \"_\" and
-- it uses a type application to generate the configuration source tree
-- representation.
--
-- @printDotEnv \@AppConfig ".env"@
--
-- @since 0.0.1.0
printDotEnv :: forall a. (ConfigSource a) => FilePath -> IO ()
printDotEnv path =
  writeFile path
    . foldMap (\line -> "export " <> line <> "=\n")
    . showEnvKeys' "_"
    $ configSource @a
