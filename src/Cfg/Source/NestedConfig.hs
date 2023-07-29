module Cfg.Source.NestedConfig where

import Cfg.Options (ConfigOptions (..))
import Cfg.Source (NestedConfig (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

defaultToNestedConfig
  :: forall a. (Generic a, GConfigForest (Rep a)) => ConfigOptions -> [Tree Text]
defaultToNestedConfig opts = gToForest @(Rep a) opts

class GConfigForest (a :: Type -> Type) where
  gToForest :: ConfigOptions -> [Tree Text]

instance GConfigForest V1 where
  gToForest _ = []

instance GConfigForest U1 where
  gToForest _ = []

instance (NestedConfig a) => GConfigForest (K1 R a) where
  gToForest _ = toNestedConfig @a

instance (GConfigForest f) => GConfigForest (M1 D s f) where
  gToForest opts = gToForest @f opts

instance (Constructor c, GConfigForest f) => GConfigForest (M1 C c f) where
  gToForest opts = gToForest @f opts

instance (Selector s, GConfigForest f) => GConfigForest (M1 S s f) where
  gToForest opts =
    if selName m == ""
      then error "Can only create a tree for named product types i.e. Records with named fields"
      else
        [ Node
            (configOptionsLabelModifier opts $ T.pack (selName m))
            (gToForest @f opts)
        ]
   where
    m :: t s f a
    m = undefined

instance (GConfigForest a, GConfigForest b) => GConfigForest (a :*: b) where
  gToForest opts = gToForest @a opts <> gToForest @b opts

instance GConfigForest (a :+: b) where
  gToForest _ = []
