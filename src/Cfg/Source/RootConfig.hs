module Cfg.Source.RootConfig where

import Cfg.Options (RootOptions (..))
import Cfg.Source.NestedConfig
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

defaultToRootConfig :: forall a. (Generic a, GConfigTree (Rep a)) => RootOptions -> Proxy a -> Tree Text
defaultToRootConfig opts _ = gToTree opts (Proxy :: Proxy (Rep a))

class GConfigTree (a :: Type -> Type) where
    gToTree :: RootOptions -> Proxy a -> Tree Text

-- TODO: Investigate whether this is required or not. Theoretically don't need this?
--
-- instance RootConfig a => GConfigTree (K1 R a) where
--   gToTree opts _ = toRootConfig opts (Proxy @a)

instance GConfigTree f => GConfigTree (M1 S s f) where
    gToTree opts _ = gToTree opts (Proxy @f)

instance GConfigTree f => GConfigTree (M1 D s f) where
    gToTree opts _ = gToTree opts (Proxy @f)

instance (Constructor c, GConfigForest f) => GConfigTree (M1 C c f) where
    gToTree opts _
        | conIsRecord m =
            Node
                (rootOptionsLabelModifier opts . T.pack $ conName m)
                (gToForest (rootOptionsFieldOptions opts) (Proxy @f))
        | otherwise = error "Can only create a tree for named product types i.e. Records with named fields"
      where
        m :: t c f a
        m = undefined
