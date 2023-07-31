module Cfg.Source.Config where

import Cfg.Options 
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import KeyTree
import Cfg.Source
import Data.Map.Strict (empty, singleton)
import Cfg.Source.Default

defaultConfigSource :: forall a. (DefaultSource a, Generic a, GConfigSource (Rep a)) => ConfigOptions -> KeyTree Text Text
defaultConfigSource opts = gConfigSource @(Rep a) (defaults @a) opts

class GConfigSource (a :: Type -> Type) where
  gConfigSource :: (Text -> Maybe Text) -> ConfigOptions -> KeyTree Text Text

instance ConfigSource a => GConfigSource (K1 R a) where
  gConfigSource _ _ = configSource @a

instance (Selector s, GConfigSource f) => GConfigSource (M1 S s f) where
  gConfigSource def opts =
    if selName @s undefined == ""
      then error "Can only create a tree for named product types i.e. Records with named fields"
      else 
        case def selectorName of 
          Nothing -> Free $ singleton key value
          Just val -> Free $ singleton key (Pure val)
    where
      selectorName :: Text
      selectorName = T.pack $ selName @s undefined 

      key :: Text
      key = keyModifier opts $ selectorName 

      value :: KeyTree Text Text 
      value = gConfigSource @f def opts

instance (Constructor c, GConfigSource f) => GConfigSource (M1 C c f) where
  gConfigSource def opts =
    case opts of
      Root (RootOptions { rootOptionsRootKey = ConstructorName modifier }) -> 
         if conIsRecord @c undefined 
            then Free $ singleton (modifier key) (gConfigSource @f def opts)
            -- TODO: Would be nice if we could turn this into a compile time error.
            else error "Can only create a tree for named product types i.e. Records with named fields"
      _ -> (gConfigSource @f def opts)
    where
      key :: Text
      key = T.pack $ conName @c undefined

instance (Datatype d, GConfigSource f) => GConfigSource (M1 D d f) where
  gConfigSource def opts =
    case opts of
      Root (RootOptions { rootOptionsRootKey = TypeName modifier }) -> 
        Free $ singleton (modifier key) (gConfigSource @f def opts)
      _ -> (gConfigSource @f def opts)
    where
      key :: Text
      key = T.pack $ datatypeName @d undefined

instance (GConfigSource a, GConfigSource b) => GConfigSource (a :*: b) where
  gConfigSource def opts = 
    case (gConfigSource @a def opts, gConfigSource @b def opts) of
      (Free m, Free m') -> Free $ m <> m'
      -- TODO: Would be nice if we could turn this into a compile time error.
      _ -> error "expected product types to generate subtrees (i.e. not contain Pure values)"

instance GConfigSource (a :+: b) where
  gConfigSource _ _ = Free empty
