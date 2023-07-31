module Cfg.Env where

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

-- | @since 0.0.1.0
envSourceSep
  :: forall m
   . (MonadIO m)
  => Text
  -> KeyTree Text Text
  -> m (KeyTree Text Text)
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

-- | @since 0.0.1.0
envSource :: (MonadFail m, MonadIO m) => KeyTree Text Text -> m (KeyTree Text Text)
envSource = envSourceSep "_"

-- | @since 0.0.1.0
printDotEnv' :: FilePath -> Text -> KeyTree Text Text -> IO ()
printDotEnv' path sep = writeFile path . foldMap (\line -> "export " <> line <> "=\n") . showEnvKeys' sep

-- | @since 0.0.1.0
getEnvConfigSep
  :: (MonadFail m, MonadIO m, ConfigSource a, ConfigParser a) => Text -> m (Either ConfigParseError a)
getEnvConfigSep sep = getConfig $ envSourceSep sep

-- | @since 0.0.1.0
getEnvConfig
  :: (MonadFail m, MonadIO m, ConfigSource a, ConfigParser a) => m (Either ConfigParseError a)
getEnvConfig = getConfig $ envSource

-- | @since 0.0.1.0
printDotEnv :: forall a. (ConfigSource a) => FilePath -> IO ()
printDotEnv path =
  writeFile path
    . foldMap (\line -> "export " <> line <> "=\n")
    . showEnvKeys' "_"
    $ configSource @a
