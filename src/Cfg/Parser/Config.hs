-- |
--  Module      : Cfg.Parser.Config
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains the generic machinery for building parsers from the
-- structure of a type. The majority of the work here is threading a
-- 'Cfg.Parser.Value.ValueParser' through the 'KeyTree.KeyTree' structure until
-- a 'Pure' value is hit, and then dispatching the correct parser.
module Cfg.Parser.Config
  ( -- * Default Parser Function
    defaultParseConfig

    -- * Generic Machinery
  , GConfigParser (..)
  )
where

import Cfg.Options
import Cfg.Parser (ConfigParseError (..), ConfigParser (..))
import Data.Kind (Type)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import KeyTree

-- | This function is the workhorse of the generic machinery, however the user
-- should never have to invoke it directly. Instead, one of the newtypes from
-- 'Cfg.Deriving.Config' should call into this function in the definition of a
-- 'Cfg.Parser.ConfigParser' instance. The deriving via type should pull out
-- the 'ConfigOptions' from type level information.
--
-- @since 0.0.1.0
defaultParseConfig
  :: forall a
   . (Generic a, GConfigParser (Rep a))
  => ConfigOptions
  -> KeyTree Text Text
  -> Either ConfigParseError a
defaultParseConfig opts tree = fmap to $ gParseConfig opts tree

-- | This class is the generic version of 'ConfigParser'. It recurses on the
-- generic structure of a type, building up a return type for the parser.
--
-- @since 0.0.2.0
class GConfigParser (f :: Type -> Type) where
  gParseConfig :: ConfigOptions -> KeyTree Text Text -> Either ConfigParseError (f p)

-- | This is the \"base case\", since "GHC.Generics" don't recurse the generic
-- represetation multiple levels, @a@ is just a plain type. Therefore we call
-- 'parseConfig' on it. @a@ may be another nested record, in which case
-- 'gParseConfig' will probably get called again, but for the generic
-- representation of a sub-tree. Or it will find the default instance for
-- 'ConfigParser' (indicating that we have reached a leaf) and dispatch to a
-- value parser through 'parseConfig'.
--
-- @since 0.0.2.0
instance (ConfigParser a) => GConfigParser (K1 R a) where
  gParseConfig _ kt = K1 <$> parseConfig kt

-- | This is the type constructor case, if we are dealing with a
-- 'Cfg.Deriving.Config.ConfigRoot' instance, then we have lookup the \"root
-- key\", but in all other cases we just keep recursing.
--
-- @since 0.0.2.0
instance (GConfigParser f, Datatype d) => GConfigParser (M1 D d f) where
  gParseConfig opts t@(Free keyForest) =
    case opts of
      Root (RootOptions{rootOptionsRootKey = TypeName modifier}) ->
        let
          key = modifier . T.pack $ datatypeName @d undefined
        in
          case M.lookup key keyForest of
            Just subTree -> M1 <$> gParseConfig opts subTree
            Nothing -> Left $ MissingKeys [key] t
      _ -> M1 <$> gParseConfig opts t
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
   where
    key = keyModifier opts . T.pack $ datatypeName @d undefined

-- | This is the data constructor case, if we are dealing with a
-- 'Cfg.Deriving.Config.ConfigRoot' instance, then we have lookup the \"root
-- key\", but in all other cases we just keep recursing.
--
-- @since 0.0.2.0
instance (Constructor c, GConfigParser f) => GConfigParser (M1 C c f) where
  gParseConfig opts t@(Free keyForest) =
    case opts of
      Root (RootOptions{rootOptionsRootKey = ConstructorName modifier}) ->
        let
          key = modifier . T.pack $ conName @c undefined
        in
          case M.lookup key keyForest of
            Just subTree -> M1 <$> gParseConfig opts subTree
            Nothing -> Left $ MissingKeys [key] t
      _ -> M1 <$> gParseConfig opts t
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
   where
    key = keyModifier opts . T.pack $ conName @c undefined

-- | This is the most important case, we need to look up the subconfig by key
-- (just the record field with all key modifiers applied), and then recursively
-- parse the sub tree.
--
-- @since 0.0.2.0
instance (Selector s, GConfigParser f) => GConfigParser (M1 S s f) where
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
   where
    key = keyModifier opts . T.pack $ selName @s undefined
  gParseConfig opts t@(Free keyForest) =
    case M.lookup selectorName keyForest of
      Nothing -> Left $ MissingKeys [selectorName] t
      Just subTree -> M1 <$> gParseConfig opts subTree
   where
    selectorName = keyModifier opts . T.pack $ selName @s undefined

-- | This is the product case, we just distribute the parsers over the
-- different product fields.
--
-- Notably, there is no sum type case. We could potentially add that in the future,
-- allowing users to specify different cases of configuration. But right now
-- that seems like it would be more confusing than helpful, so we just give a
-- type error by eliding the instance.
--
-- @since 0.0.2.0
instance (GConfigParser a, GConfigParser b) => GConfigParser (a :*: b) where
  gParseConfig opts xs = do
    a <- gParseConfig opts xs
    b <- gParseConfig opts xs
    pure $ a :*: b
