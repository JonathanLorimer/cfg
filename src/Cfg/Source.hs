{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
module Cfg.Source where

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

class RootConfig a where
  toRootConfig :: Proxy a -> Tree Text

  default toRootConfig :: (Generic a, GConfigTree1 (Rep a)) => Proxy a -> Tree Text
  toRootConfig _ = defaultToTree (Proxy @a)

defaultToTree :: forall a . (Generic a, GConfigTree1 (Rep a)) => Proxy a -> Tree Text
defaultToTree _ = gToTree1 (Proxy :: Proxy (Rep a))

class GConfigTree1 (a :: Type -> Type) where
  gToTree1 :: Proxy a -> Tree Text

instance RootConfig a => GConfigTree1 (K1 R a) where
  gToTree1 _ = toRootConfig (Proxy @a)

instance GConfigTree1 f => GConfigTree1 (M1 D s f) where
  gToTree1 _ = gToTree1 (Proxy @f)

instance (Constructor c, GConfigForest1 f) => GConfigTree1 (M1 C c f) where
  gToTree1 _ 
    | conIsRecord m = Node (T.pack $ conName m) (gToForest1 (Proxy @f))
    | otherwise = error "Can only create a tree for named product types i.e. Records with named fields"
    where
      m :: t c f a
      m = undefined

-- instance (Selector s, GConfigForest1 f) => GConfigTree1 (M1 S s f) where
--   gToTree1 _ =
--     if selName m == ""
--       then error "Can only create a tree for named product types i.e. Records with named fields"
--       else Node (T.pack (selName m)) (gToForest1 $ Proxy @f)
--     where
--       m :: t s f a
--       m = undefined
--
-- instance GConfigTree1 (a :+: b) where
--   gToTree1 _ = undefined


class NestedConfig a where
  toNestedConfig :: Proxy a -> [Tree Text]

  default toNestedConfig :: (Generic a, GConfigForest1 (Rep a)) => Proxy a -> [Tree Text]
  toNestedConfig _ = defaultToNestedConfig (Proxy @a)

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

instance (GConfigForest1 a, GConfigForest1 b) => GConfigForest1 (a :*: b) where
  gToForest1 _ = gToForest1 (Proxy @a) <> gToForest1 (Proxy @b)

instance (Selector s, GConfigForest1 f) => GConfigForest1 (M1 S s f) where
  gToForest1 _ =
    if selName m == ""
      then error "Can only create a tree for named product types i.e. Records with named fields"
      else [Node (T.pack (selName m)) (gToForest1 $ Proxy @f)]
    where
      m :: t s f a
      m = undefined

instance GConfigForest1 (a :+: b) where
  gToForest1 _ = []
