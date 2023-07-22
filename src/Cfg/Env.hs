module Cfg.Env where

import Prelude hiding (writeFile)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree, levels, foldTree)
import Data.Traversable (for)
import System.Environment (lookupEnv)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Tree (Tree(..))
import Data.Foldable
import Data.List (intersperse)
import Tree.Append (travAppendLeafA)
import Data.Text.IO (writeFile)

envSourceSep 
  :: forall m . (MonadFail m, MonadIO m) 
  => Text 
  -> Tree Text 
  -> m (Tree Text)
envSourceSep sep = travAppendLeafA getLeafFromEnv []
  where 
    getLeafFromEnv :: [Text] -> m Text
    getLeafFromEnv keys = do 
      let key = foldr (flip mappend) "" $ intersperse sep keys

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
    f label xs = concat $ fmap (label:) <$> xs

showEnvKeys :: Text -> Tree Text -> [Text]
showEnvKeys sep tree = getEnvKey sep <$> getKeys tree

printDotEnv :: FilePath -> Text -> Tree Text -> IO ()
printDotEnv path sep = writeFile path . foldMap (\line -> "export " <> line <> "=\n") . showEnvKeys sep

envSource :: (MonadFail m, MonadIO m) => Tree Text -> m (Tree Text)
envSource = envSourceSep "_"
