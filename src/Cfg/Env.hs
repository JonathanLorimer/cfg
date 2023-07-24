module Cfg.Env where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (writeFile)
import Data.Tree (Tree, foldTree)
import System.Environment (lookupEnv)
import Tree.Append (travAppendLeafA)
import Prelude hiding (writeFile)

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

getEnvKey :: Text -> [Text] -> Text
getEnvKey sep = foldr mappend "" . intersperse sep

getKeys :: Tree Text -> [[Text]]
getKeys = foldTree f
 where
  f :: Text -> [[[Text]]] -> [[Text]]
  f label [] = [[label]]
  f label xs = concat $ fmap (label :) <$> xs

showEnvKeys :: Text -> Tree Text -> [Text]
showEnvKeys sep tree = getEnvKey sep <$> getKeys tree

printDotEnv :: FilePath -> Text -> Tree Text -> IO ()
printDotEnv path sep = writeFile path . foldMap (\line -> "export " <> line <> "=\n") . showEnvKeys sep

envSource :: (MonadFail m, MonadIO m) => Tree Text -> m (Tree Text)
envSource = envSourceSep "_"
