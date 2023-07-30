module Cfg.Source.Config where

import Cfg.Options 
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import KeyTree
import Cfg.Source
import Data.Map.Strict (empty, singleton)

defaultConfigSource :: forall a. (Generic a, GConfigSource (Rep a)) => ConfigOptions -> KeyTree Text Text
defaultConfigSource opts = gConfigSource @(Rep a) opts

class GConfigSource (a :: Type -> Type) where
  gConfigSource :: ConfigOptions -> KeyTree Text Text

instance ConfigSource a => GConfigSource (K1 R a) where
  gConfigSource _ = configSource @a

instance (Selector s, GConfigSource f) => GConfigSource (M1 S s f) where
  gConfigSource opts =
    if selName @s undefined == ""
      then error "Can only create a tree for named product types i.e. Records with named fields"
      else Free $ singleton key value
    where
      key :: Text
      key = keyModifier opts . T.pack $ selName @s undefined 

      value :: KeyTree Text Text 
      value = gConfigSource @f opts

instance (Constructor c, GConfigSource f) => GConfigSource (M1 C c f) where
  gConfigSource opts =
    case opts of
      Root (RootOptions { rootOptionsRootKey = DataCon modifier }) -> 
         if conIsRecord @c undefined 
            then Free $ singleton (modifier key) (gConfigSource @f opts)
            else error "Can only create a tree for named product types i.e. Records with named fields"
      _ -> (gConfigSource @f opts)
    where
      key :: Text
      key = T.pack $ conName @c undefined

instance (Datatype d, GConfigSource f) => GConfigSource (M1 D d f) where
  gConfigSource opts =
    case opts of
      Root (RootOptions { rootOptionsRootKey = TyCon modifier }) -> 
        Free $ singleton (modifier key) (gConfigSource @f opts)
      _ -> (gConfigSource @f opts)
    where
      key :: Text
      key = T.pack $ datatypeName @d undefined

instance (GConfigSource a, GConfigSource b) => GConfigSource (a :*: b) where
  gConfigSource opts = 
    case (gConfigSource @a opts, gConfigSource @b opts) of
      (Free m, Free m') -> Free $ m <> m'
      _ -> error "expected product types to generate subtrees (i.e. not contain Pure values)"

instance GConfigSource (a :+: b) where
  gConfigSource _ = Free empty
