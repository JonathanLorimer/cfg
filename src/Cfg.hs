module Cfg where

import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO)
import Data.Tree (Tree)

getConfig :: Monad m => 
  Tree Text -> 
  (Tree Text -> m (Tree Text)) -> 
  (Tree Text -> Either e a) -> 
  m (Either e a)
getConfig keyTree source parser = parser <$> source keyTree

