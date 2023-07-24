module Cfg.Source.RootConfig where

import Cfg.Options (RootOptions (..))
import Cfg.Source.NestedConfig
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

defaultToRootConfig :: forall a. (Generic a, GConfigTree (Rep a)) => RootOptions -> Tree Text
defaultToRootConfig opts = gToTree @(Rep a) opts

class GConfigTree (a :: Type -> Type) where
    gToTree :: RootOptions -> Tree Text

-- TODO: Investigate whether this is required or not. Theoretically don't need this?
--
-- instance RootConfig a => GConfigTree (K1 R a) where
--   gToTree opts _ = toRootConfig opts (Proxy @a)

instance GConfigTree f => GConfigTree (M1 S s f) where
    gToTree opts = gToTree @f opts

instance GConfigTree f => GConfigTree (M1 D s f) where
    gToTree opts = gToTree @f opts

instance (Constructor c, GConfigForest f) => GConfigTree (M1 C c f) where
    gToTree opts
        | conIsRecord m =
            Node
                (rootOptionsLabelModifier opts . T.pack $ conName m)
                (gToForest @f $ rootOptionsFieldOptions opts)
        | otherwise = error "Can only create a tree for named product types i.e. Records with named fields"
      where
        m :: t c f a
        m = undefined
