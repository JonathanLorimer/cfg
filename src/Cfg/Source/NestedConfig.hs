module Cfg.Source.NestedConfig where

import Cfg.Options (ConfigOptions (..))
import Cfg.Source (NestedConfig (..))
import Data.Data (Proxy (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

defaultToNestedConfig :: forall a. (Generic a, GConfigForest (Rep a)) => ConfigOptions -> Proxy a -> [Tree Text]
defaultToNestedConfig opts _ = gToForest opts (Proxy :: Proxy (Rep a))

{- | Generic typeclass machinery for inducting on the structure
 of the type, such that we can thread `Display` instances through
 the structure of the type. The primary use case is for implementing
 `RecordInstance`, which does this "threading" for record fields. This
 machinery does, crucially, depend on child types (i.e. the type of a
 record field) having a `Display` instance.

 @since 0.0.1.0
-}
class GConfigForest (a :: Type -> Type) where
    gToForest :: ConfigOptions -> Proxy a -> [Tree Text]

instance GConfigForest V1 where
    gToForest _ _ = []

instance GConfigForest U1 where
    gToForest _ _ = []

instance NestedConfig a => GConfigForest (K1 R a) where
    gToForest _ _ = toNestedConfig (Proxy @a)

instance GConfigForest f => GConfigForest (M1 D s f) where
    gToForest opts _ = gToForest opts (Proxy @f)

instance (Constructor c, GConfigForest f) => GConfigForest (M1 C c f) where
    gToForest opts _ = gToForest opts (Proxy @f)

instance (Selector s, GConfigForest f) => GConfigForest (M1 S s f) where
    gToForest opts _ =
        if selName m == ""
            then error "Can only create a tree for named product types i.e. Records with named fields"
            else
                [ Node
                    (configOptionsLabelModifier opts $ T.pack (selName m))
                    (gToForest opts $ Proxy @f)
                ]
      where
        m :: t s f a
        m = undefined

instance (GConfigForest a, GConfigForest b) => GConfigForest (a :*: b) where
    gToForest opts _ = gToForest opts (Proxy @a) <> gToForest opts (Proxy @b)

instance GConfigForest (a :+: b) where
    gToForest _ _ = []
