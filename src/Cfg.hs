module Cfg (
  module Cfg.Parser,
  module Cfg.Source,
  getConfigRaw,
  getConfig,
) where

import Data.Text (Text)
import Data.Tree (Tree)
import Cfg.Source (RootConfig(..), FetchSource)
import Cfg.Parser (RootParser(..), ConfigParseError)

-- | @since 0.0.1.0
getConfigRaw ::
    Monad m =>
    Tree Text ->
    (Tree Text -> m (Tree Text)) ->
    (Tree Text -> Either e a) ->
    m (Either e a)
getConfigRaw keyTree source parser = parser <$> source keyTree

-- | @since 0.0.1.0
getConfig :: forall a m . (Monad m, RootConfig a, RootParser a) => FetchSource m -> m (Either ConfigParseError a)
getConfig fetch = parseRootConfig @a <$> fetch (toRootConfig @a)
