module Cfg.Source.RootConfig where

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
import Cfg.Source.NestedConfig
import Cfg.Source (RootConfig(..))

defaultToRootConfig :: forall a . (Generic a, GConfigTree1 (Rep a)) => Proxy a -> Tree Text
defaultToRootConfig _ = gToTree1 (Proxy :: Proxy (Rep a))

class GConfigTree1 (a :: Type -> Type) where
  gToTree1 :: Proxy a -> Tree Text

instance RootConfig a => GConfigTree1 (K1 R a) where
  gToTree1 _ = toRootConfig (Proxy @a)

instance GConfigTree1 f => GConfigTree1 (M1 S s f) where
  gToTree1 _ = gToTree1 (Proxy @f)

instance GConfigTree1 f => GConfigTree1 (M1 D s f) where
  gToTree1 _ = gToTree1 (Proxy @f)

instance (Constructor c, GConfigForest1 f) => GConfigTree1 (M1 C c f) where
  gToTree1 _ 
    | conIsRecord m = Node (T.pack $ conName m) (gToForest1 (Proxy @f))
    | otherwise = error "Can only create a tree for named product types i.e. Records with named fields"
    where
      m :: t c f a
      m = undefined
