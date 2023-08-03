-- |
--  Module      : Cfg.Parser.Value
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains the generic machinery for value parsers. The main use
-- case for deriving 'Cfg.Parser.ValueParser' generically is for sum types, as
-- instances for most common types are provided in "Cfg.Parser".
module Cfg.Parser.Value where

import Cfg.Parser
import Data.Kind (Type)
import Data.Text qualified as T
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char (string)

-- | This is the function that hooks into the generic machinery. It is called
-- by the deriving mechanism in "Cfg.Deriving.Value".
--
-- @since 0.0.2.0
defaultValueParser
  :: forall a
   . (Generic a, GValueParser (Rep a))
  => Parser a
defaultValueParser = fmap to $ gParser @(Rep a)

-- | This is a generic version of 'Cfg.Parser.ValueParser'
--
-- @since 0.0.2.0
class GValueParser (f :: Type -> Type) where
  gParser :: Parser (f p)

-- | @since 0.0.2.0
instance GValueParser V1 where
  gParser = undefined

-- | @since 0.0.2.0
instance GValueParser U1 where
  gParser = string "()" >> pure U1

-- | @since 0.0.2.0
instance (ValueParser a) => GValueParser (K1 R a) where
  gParser = K1 <$> parser @a

-- | @since 0.0.2.0
instance (GValueParser f) => GValueParser (M1 D s f) where
  gParser = M1 <$> gParser @f

-- | @since 0.0.2.0
instance (Constructor c) => GValueParser (M1 C c U1) where
  gParser = M1 U1 <$ string (T.pack $ conName @c undefined)

-- | @since 0.0.2.0
instance (GValueParser f) => GValueParser (M1 S s f) where
  gParser = M1 <$> gParser @f

-- | This is the main instance, which distributs a value parser over a sum type
-- using retry and alternative.
--
-- @since 0.0.2.0
instance (GValueParser a, GValueParser b) => GValueParser (a :+: b) where
  gParser = L1 <$> (try $ gParser @a) <|> R1 <$> (gParser @b)

-- | This is also an important instance for product types with unnamed fields
-- that are intended to be parsed as values (not nested configurations).
--
-- @since 0.0.2.0
instance (GValueParser a, GValueParser b) => GValueParser (a :*: b) where
  gParser = liftA2 (:*:) (gParser @a) (gParser @b)
