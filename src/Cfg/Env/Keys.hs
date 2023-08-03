-- |
--  Module      : Cfg.Env.Keys
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module provides helper functions for manipulating the keys on our
-- internal tree representation of a configuration; 'Cfg.KeyTree.KeyTree'.
--
-- These helper functions are generally oriented towards using environment
-- variables as your configuration source.
module Cfg.Env.Keys where

import Cfg.Source (ConfigSource (..))
import Data.List (intersperse)
import Data.Map.Strict (empty)
import Data.Text (Text)
import KeyTree
import Data.Foldable

-- | This function takes a separator and a list of keys and joins them from the
-- end of the list to the beginning, interspersed with the provided separator.
--
-- >>> getEnvKey "_" ["A", "B", "C"]
-- "A_B_C"
--
-- @since 0.0.1.0
getEnvKey 
  :: Text -- ^ Separator
  -> [Text] -- ^ List of keys
  -> Text
getEnvKey sep = fold . intersperse sep

-- | Folds a 'Cfg.KeyTree.KeyTree' from leaf to root, into distinct key paths.
-- This is necessary for the way that hierarchical structures are represented
-- in environment variables (i.e. \"KEYA_SUBKEYA\", \"KEYA_SUBKEYB\").
--
-- Here is a visual representation of how the keys would get folded
--
-- @
--       A
--      / \\
--     B   C
--
--  [ [ "A", "B" ]
--  , [ "A", "C" ] 
--  ]
-- @
-- >>> import KeyTree
-- >>> import Data.Map qualified as M
-- >>> getKeys $ Free $ M.singleton "A" $ Free (M.fromList [("B", Free M.empty), ("C", Free M.empty)])
-- [["A","B"],["A","C"]]
--
-- @since 0.0.1.0
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

-- | Gets all the keys from a configuration tree, and flattens the hierarchy so that
-- each key is prefixed with its path through the tree.
--
-- Accepts separator to individuate the key prefixes.
--
-- >>> import KeyTree
-- >>> import Data.Map qualified as M
-- >>> showEnvKeys' "-" $ Free $ M.singleton "A" $ Free (M.fromList [("B", Free M.empty), ("C", Free M.empty)])
-- ["A-B","A-C"]
--
-- @since 0.0.1.0
showEnvKeys' 
  :: Text -- ^ Separator
  -> KeyTree Text Text -- ^ Configuration tree
  -> [Text]
showEnvKeys' sep tree = getEnvKey sep <$> getKeys tree

-- | Same as 'showEnvKeys'' but the 'KeyTree.KeyTree' is generated via a 'configSource'
--
-- >>> import GHC.Generics (Generic (..))
-- >>> import Cfg.Source (ConfigSource(..))
-- >>> import Cfg.Parser (ConfigParser(..))
-- >>> import Cfg.Deriving.Config (Config(..))
-- >>> import Cfg.Source.Default (DefaultSource(..))
-- >>> :{
-- data Sub = Sub { c :: Int, d :: Bool } 
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser) via Config Sub
-- data TypeCon = DataCon { a :: Sub, b :: Int }
--   deriving (Generic, Show, DefaultSource)
--   deriving (ConfigSource, ConfigParser) via Config TypeCon
-- :}
--
-- >>> showEnvKeys @TypeCon "_"
-- ["a_c","a_d","b"]
--
-- @since 0.0.1.0
showEnvKeys :: forall a. (ConfigSource a) => Text -> [Text]
showEnvKeys sep = getEnvKey sep <$> (getKeys $ configSource @a)
