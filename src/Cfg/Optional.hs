module Cfg.Optional where

import Cfg.Parser (ConfigParser (..), ConfigParseError (ExpectedForestFoundValue))
import Cfg.Source (ConfigSource (..))
import Control.Monad.Free
import Data.Map.Strict qualified as M
import KeyTree

-- True when no leaf in the subtree carries a value.
allAbsent :: KeyTree k v -> Bool
allAbsent (Free m) = all allAbsent (M.elems m)
allAbsent (Pure _) = False

newtype OptionalConfig a = OptionalConfig {getOptionalConfig :: Maybe a}
  deriving newtype (Eq, Ord, Show)

instance ConfigParser a => ConfigParser (OptionalConfig a) where
  parseConfig cfg@(Free _) =
    if allAbsent cfg
      then Right $ OptionalConfig Nothing
      else OptionalConfig . Just <$> parseConfig cfg
  parseConfig (Pure val) = Left $ ExpectedForestFoundValue val

instance ConfigSource a => ConfigSource (OptionalConfig a) where
  configSource = configSource @a
