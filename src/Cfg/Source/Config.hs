-- |
--  Module      : Cfg.Source.Config
--  Copyright   : © Jonathan Lorimer, 2023
--  License     : MIT
--  Maintainer  : jonathanlorimer@pm.me
--  Stability   : stable
--
-- @since 0.0.2.0
--
-- This module contains the generic machinery for generating a tree
-- representation of your configuration, this tree representation is intended
-- to be used with various sources. The tree structure should match the
-- structure of your (potentially) nested record type.
--
-- It is important to note that defaults are injected here, and not in the
-- parser stage. We use a 'DefaultSource' instance to inject 'Pure' values at
-- the leaves that can be used if the source fetcher doesn't return a value for
-- that key. In the case that there is no default a 'Free Data.Map.empty' is
-- placed to represent a required value.
module Cfg.Source.Config
  ( -- * Default Source Generator
    defaultConfigSource

    -- * Generic Machinery
  , GConfigSource (..)
  )
where

import Cfg.Options
import Cfg.Source
import Cfg.Source.Default
import Data.Kind (Type)
import Data.Map.Strict (empty, singleton)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import KeyTree

-- | This function is used by the deriving via machinery to dispatch to the
-- generic machinery, the user should never have to invoke it directly. This
-- takes in a 'ConfigOptions' which are retrieved from the deriving via
-- newtypes, and also threads a 'DefaultSource' instance through so that we can
-- dispatch to 'defaults' in the 'K1' case.
--
-- @since 0.0.2.0
defaultConfigSource
  :: forall a. (DefaultSource a, Generic a, GConfigSource (Rep a)) => ConfigOptions -> KeyTree Text Text
defaultConfigSource opts = gConfigSource @(Rep a) (defaults @a) opts

-- | This class is the generic version of 'ConfigSource. It recurses on the
-- generic structure of a type, building up 'KeyTree' representation.
--
-- @since 0.0.2.0
class GConfigSource (a :: Type -> Type) where
  gConfigSource :: (Text -> Maybe Text) -> ConfigOptions -> KeyTree Text Text

-- | This is the \"base case\", since "GHC.Generics" don't recurse the generic
-- represetation multiple levels, @a@ is just a plain type. Therefore we call
-- 'configSource' on it. @a@ may be another nested record, in which case
-- 'Cfg.Parser.Config.gParseConfig' will probably get called again, but for the
-- generic representation of a sub-tree. It will do this until it finds a
-- 'ConfigSource' instance for 'Cfg.Deriving.Value.Value' which will just add a
-- 'Free Data.Map.empty' (indicating a hole to be filled when we fetch the
-- configuration).
--
-- @since 0.0.2.0
instance (ConfigSource a) => GConfigSource (K1 R a) where
  gConfigSource _ _ = configSource @a

-- | This instance is important because it does the work of pulling off the
-- field selector name, and creating a sub-tree under that key by calling
-- 'gConfigSource' recursively. If there is a default for that selector then no
-- sub-tree is created, instead we insert a \"placeholder\" value tagged by
-- 'Pure' to represent that it is the end of the tree.
--
-- We detect if a default exists by calling 'defaults' from the 'DefaultSource'
-- instance on the selector, @defaults@ is of type @Text -> Maybe Text@, so the
-- @Nothing@ case indicates no default.
--
-- @since 0.0.2.0
instance (Selector s, GConfigSource f) => GConfigSource (M1 S s f) where
  gConfigSource def opts =
    if selName @s undefined == ""
      then -- TODO: Would be nice if we could turn this into a compile time error.
        error "Can only create a tree for named product types i.e. Records with named fields"
      else case def selectorName of
        Nothing -> Free $ singleton key value
        Just val -> Free $ singleton key (Pure val)
   where
    selectorName :: Text
    selectorName = T.pack $ selName @s undefined

    key :: Text
    key = keyModifier opts $ selectorName

    value :: KeyTree Text Text
    value = gConfigSource @f def opts

-- | This is the data constructor case, if we are dealing with a
-- 'Cfg.Deriving.Config.ConfigRoot' instance, then we have to create an extra layer with the \"root
-- key\" as a key and then a subtree (calculated by recursively calling 'gConfigSource') as the value.
--
-- @since 0.0.2.0
instance (Constructor c, GConfigSource f) => GConfigSource (M1 C c f) where
  gConfigSource def opts =
    case opts of
      Root (RootOptions{rootOptionsRootKey = ConstructorName modifier}) ->
        if conIsRecord @c undefined
          then Free $ singleton (modifier key) (gConfigSource @f def opts)
          else -- TODO: Would be nice if we could turn this into a compile time error.
            error "Can only create a tree for named product types i.e. Records with named fields"
      _ -> (gConfigSource @f def opts)
   where
    key :: Text
    key = T.pack $ conName @c undefined

-- | This is the type constructor case, if we are dealing with a
-- 'Cfg.Deriving.Config.ConfigRoot' instance, then we have to create an extra layer with the \"root
-- key\" as a key and then a subtree (calculated by recursively calling 'gConfigSource') as the value.
--
-- @since 0.0.2.0
instance (Datatype d, GConfigSource f) => GConfigSource (M1 D d f) where
  gConfigSource def opts =
    case opts of
      Root (RootOptions{rootOptionsRootKey = TypeName modifier}) ->
        Free $ singleton (modifier key) (gConfigSource @f def opts)
      _ -> (gConfigSource @f def opts)
   where
    key :: Text
    key = T.pack $ datatypeName @d undefined

-- | This instance handles product types and is pretty important. We need to
-- check that recursive calls to 'gConfigSource' generate sub-trees, and then
-- we merge the sub-trees.
--
-- You may wonder what happens if there is a 'Pure' value in one of the record
-- fields, well that would be represented like so:
--
-- @
-- Free $ M.singleton fieldName (Pure value)
-- @
--
-- since we need to account for the key corresponding to the record field. So
-- we really should never hit a case were a recursive call to 'gConfigSource'
-- yields a raw 'Pure'.
--
-- @since 0.0.2.0
instance (GConfigSource a, GConfigSource b) => GConfigSource (a :*: b) where
  gConfigSource def opts =
    case (gConfigSource @a def opts, gConfigSource @b def opts) of
      (Free m, Free m') -> Free $ m <> m'
      -- TODO: Would be nice if we could turn this into a compile time error.
      _ -> error "expected product types to generate subtrees (i.e. not contain Pure values)"

-- | Sum types should represent base values, so @Free M.empty@ is the right
-- thing to do here, although we should probably never hit this case, since sum
-- types should be nested under record fields as base values.
--
-- @since 0.0.2.0
instance GConfigSource (a :+: b) where
  gConfigSource _ _ = Free empty
