{-# LANGUAGE UndecidableInstances #-}
module Cfg.Deriving.ConfigValue where

import GHC.Generics
import Cfg.Parser.ValueParser
import Cfg.Parser
import Data.Coerce

newtype ConfigValue a = ConfigValue {unConfigValue :: a}

instance Generic a => Generic (ConfigValue a) where
    type Rep (ConfigValue a) = Rep a
    to = ConfigValue . to
    from (ConfigValue x) = from x

instance (Generic a, GValueParser (Rep a)) => ValueParser (ConfigValue a) where
    parser = coerce `asTypeOf` fmap ConfigValue $ defaultValueParser @a

instance (Generic a, GValueParser (Rep a)) => ConfigParser (ConfigValue a)
