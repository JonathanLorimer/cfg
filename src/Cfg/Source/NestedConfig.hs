module Cfg.Source.NestedConfig where

import Data.Tree (Tree (..))
import Data.Text (Text)
import GHC.Generics
import qualified Data.Text as T
import GHC.TypeError (ErrorMessage (..))
import GHC.TypeLits (TypeError)
import Data.Kind (Type)
import GHC.Base (Constraint)
import Data.Data (Proxy(..))
import Data.Text (Text)
import Data.Tree (Tree)
import GHC.Generics (Generic (..), Generic1 (..))
import Data.Data (Proxy(..))
import Cfg.Source (NestedConfig(..))

defaultToNestedConfig :: forall a . (Generic a, GConfigForest1 (Rep a)) => Proxy a -> [Tree Text]
defaultToNestedConfig _ = gToForest1 (Proxy :: Proxy (Rep a))

-- | Generic typeclass machinery for inducting on the structure
-- of the type, such that we can thread `Display` instances through
-- the structure of the type. The primary use case is for implementing
-- `RecordInstance`, which does this "threading" for record fields. This
-- machinery does, crucially, depend on child types (i.e. the type of a
-- record field) having a `Display` instance.
--
-- @since 0.0.1.0
class GConfigForest1 (a :: Type -> Type) where
  gToForest1 :: Proxy a -> [Tree Text]

instance GConfigForest1 V1 where
  gToForest1 _ = []

instance GConfigForest1 U1 where
  gToForest1 _ = []

instance NestedConfig a => GConfigForest1 (K1 R a) where
  gToForest1 _ = toNestedConfig (Proxy @a)

instance GConfigForest1 f => GConfigForest1 (M1 D s f) where
  gToForest1 _ = gToForest1 (Proxy @f)

instance (Constructor c, GConfigForest1 f) => GConfigForest1 (M1 C c f) where
  gToForest1 _ = gToForest1 (Proxy @f)

instance (Selector s, GConfigForest1 f) => GConfigForest1 (M1 S s f) where
  gToForest1 _ =
    if selName m == ""
      then error "Can only create a tree for named product types i.e. Records with named fields"
      else [Node (T.pack (selName m)) (gToForest1 $ Proxy @f)]
    where
      m :: t s f a
      m = undefined

instance (GConfigForest1 a, GConfigForest1 b) => GConfigForest1 (a :*: b) where
  gToForest1 _ = gToForest1 (Proxy @a) <> gToForest1 (Proxy @b)

instance GConfigForest1 (a :+: b) where
  gToForest1 _ = []


