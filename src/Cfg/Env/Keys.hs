module Cfg.Env.Keys where

import Cfg.Source (ConfigSource (..))
import Data.List (intersperse)
import Data.Map.Strict (empty)
import Data.Text (Text)
import KeyTree

-- | @since 0.0.1.0
getEnvKey :: Text -> [Text] -> Text
getEnvKey sep = foldr mappend "" . intersperse sep

-- | @since 0.0.1.0
getKeys :: KeyTree Text Text -> [[Text]]
getKeys = foldKeyTree valF stepF []
 where
  stepF :: Text -> Free (Map Text) Text -> [[Text]] -> [[Text]]
  stepF key (Pure _) acc = [key] : acc
  stepF key t@(Free m) acc =
    if m == empty
      then [key] : acc
      else fmap (key :) (getKeys t) <> acc

  valF :: Text -> [[Text]]
  valF _ = []

-- | @since 0.0.1.0
showEnvKeys' :: Text -> KeyTree Text Text -> [Text]
showEnvKeys' sep tree = getEnvKey sep <$> getKeys tree

-- | @since 0.0.1.0
showEnvKeys :: forall a. (ConfigSource a) => Text -> [Text]
showEnvKeys sep = getEnvKey sep <$> (getKeys $ configSource @a)
