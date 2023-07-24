module Cfg.Env where

import Cfg
import Cfg.Parser
import Cfg.Source
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (writeFile)
import Data.Tree (Tree, foldTree)
import System.Environment (lookupEnv)
import Tree.Append (travAppendLeafA)
import Prelude hiding (writeFile)

-- | @since 0.0.1.0
envSourceSep
  :: forall m
   . (MonadFail m, MonadIO m)
  => Text
  -> Tree Text
  -> m (Tree Text)
envSourceSep sep = travAppendLeafA getLeafFromEnv []
 where
  getLeafFromEnv :: [Text] -> m Text
  getLeafFromEnv keys = do
    let
      key = foldr (flip mappend) "" $ intersperse sep keys

    mayVal <- liftIO $ lookupEnv $ T.unpack key
    case mayVal of
      Nothing -> fail $ "Missing Key: " <> T.unpack key
      Just val -> pure $ T.pack val

-- | @since 0.0.1.0
envSource :: (MonadFail m, MonadIO m) => Tree Text -> m (Tree Text)
envSource = envSourceSep "_"

-- | @since 0.0.1.0
getEnvKey :: Text -> [Text] -> Text
getEnvKey sep = foldr mappend "" . intersperse sep

-- | @since 0.0.1.0
getKeys :: Tree Text -> [[Text]]
getKeys = foldTree f
 where
  f :: Text -> [[[Text]]] -> [[Text]]
  f label [] = [[label]]
  f label xs = concat $ fmap (label :) <$> xs

-- | @since 0.0.1.0
showEnvKeys' :: Text -> Tree Text -> [Text]
showEnvKeys' sep tree = getEnvKey sep <$> getKeys tree

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
showEnvKeys :: forall a. (RootConfig a) => Text -> [Text]
showEnvKeys sep = getEnvKey sep <$> (getKeys $ toRootConfig @a)

-- | @since 0.0.1.0
printDotEnv :: forall a. (RootConfig a) => FilePath -> Text -> IO ()
printDotEnv path sep =
  writeFile path
    . foldMap (\line -> "export " <> line <> "=\n")
    . showEnvKeys' sep
    $ toRootConfig @a
