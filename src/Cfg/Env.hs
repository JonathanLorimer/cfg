module Cfg.Env where

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

envSource :: (MonadFail m, MonadIO m) => Tree Text -> m (Tree Text)
envSource = envSourceSep "_"
