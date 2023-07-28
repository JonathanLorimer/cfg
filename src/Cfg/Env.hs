module Cfg.Env where

import Cfg
import Cfg.Env.Keys
import Cfg.Parser
import Cfg.Source (RootConfig, toRootConfig)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (writeFile)
import Data.Tree (Tree)
import System.Environment (lookupEnv)
import Tree.Append (mayAppendLeafA')
import Prelude hiding (writeFile)

-- | @since 0.0.1.0
envSourceSep
  :: forall m
   . (MonadFail m, MonadIO m)
  => Text
  -> Tree Text
  -> m (Tree Text)
envSourceSep sep = mayAppendLeafA' getLeafFromEnv []
 where
  getLeafFromEnv :: [Text] -> m (Maybe Text)
  getLeafFromEnv keys = do
    let
      key = foldr (flip mappend) "" $ intersperse sep keys

    liftIO $ (fmap T.pack) <$> (lookupEnv $ T.unpack key)

-- | @since 0.0.1.0
envSource :: (MonadFail m, MonadIO m) => Tree Text -> m (Tree Text)
envSource = envSourceSep "_"

-- | @since 0.0.1.0
printDotEnv' :: FilePath -> Text -> Tree Text -> IO ()
printDotEnv' path sep = writeFile path . foldMap (\line -> "export " <> line <> "=\n") . showEnvKeys' sep

-- | @since 0.0.1.0
getEnvConfigSep :: (MonadFail m, MonadIO m, RootConfig a, RootParser a) => Text -> m (Either ConfigParseError a)
getEnvConfigSep sep = getConfig $ envSourceSep sep

-- | @since 0.0.1.0
getEnvConfig :: (MonadFail m, MonadIO m, RootConfig a, RootParser a) => m (Either ConfigParseError a)
getEnvConfig = getConfig $ envSource

-- | @since 0.0.1.0
printDotEnv :: forall a. (RootConfig a) => FilePath -> IO ()
printDotEnv path =
  writeFile path
    . foldMap (\line -> "export " <> line <> "=\n")
    . showEnvKeys' "_"
    $ toRootConfig @a
