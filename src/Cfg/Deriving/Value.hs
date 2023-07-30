{-# LANGUAGE UndecidableInstances #-}

module Cfg.Deriving.Value where

import Cfg.Parser
import Cfg.Parser.Value
import Data.Coerce
import GHC.Generics

newtype Value a = Value {unConfigValue :: a}

instance (Generic a) => Generic (Value a) where
  type Rep (Value a) = Rep a
  to = Value . to
  from (Value x) = from x

instance (Generic a, GValueParser (Rep a)) => ValueParser (Value a) where
  parser = coerce `asTypeOf` fmap Value $ defaultValueParser @a

instance (Generic a, GValueParser (Rep a)) => ConfigParser (Value a)
