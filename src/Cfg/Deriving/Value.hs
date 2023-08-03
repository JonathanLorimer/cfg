-- |
--  Module      : Cfg.Deriving.Value
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module provides a type 'Value' for generating instances for \"leaf\"
-- elements of your configuration tree. These are the elements you actually
-- care about and want to parse out of a configuration source.
module Cfg.Deriving.Value where

import Cfg.Parser
import Cfg.Parser.Value
import Data.Coerce
import GHC.Generics

-- | This newtype is used to derive 'ValueParser' instances for your types
-- using the deriving via mechanism. In general this should be used for sum
-- types, and product types without named fields (i.e. not records). The
-- majority of the types that you would want as values should have instances in
-- "Cfg.Source" and "Cfg.Parser".
--
-- @since 0.0.2.0
newtype Value a = Value {unValue :: a}

instance (Generic a) => Generic (Value a) where
  type Rep (Value a) = Rep a
  to = Value . to
  from (Value x) = from x

instance (Generic a, GValueParser (Rep a)) => ValueParser (Value a) where
  parser = coerce `asTypeOf` fmap Value $ defaultValueParser @a

instance (Generic a, GValueParser (Rep a)) => ConfigParser (Value a)
