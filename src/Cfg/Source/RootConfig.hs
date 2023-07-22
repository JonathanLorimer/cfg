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

data RootOptions =
  RootOptions  
    { rootOptionsLabelModifier :: Text -> Text 
    , rootOptionsFieldOptions :: ConfigOptions
    }

defaultRootOptions :: RootOptions
defaultRootOptions = RootOptions id (ConfigOptions id)

defaultToRootConfig :: forall a . (Generic a, GConfigTree1 (Rep a)) => RootOptions -> Proxy a -> Tree Text
defaultToRootConfig opts _ = gToTree1 opts (Proxy :: Proxy (Rep a))

class GConfigTree1 (a :: Type -> Type) where
  gToTree1 :: RootOptions -> Proxy a -> Tree Text

-- Theoretically don't need this?
-- instance RootConfig a => GConfigTree1 (K1 R a) where
--   gToTree1 opts _ = toRootConfig opts (Proxy @a)

instance GConfigTree1 f => GConfigTree1 (M1 S s f) where
  gToTree1 opts _ = gToTree1 opts (Proxy @f)

instance GConfigTree1 f => GConfigTree1 (M1 D s f) where
  gToTree1 opts _ = gToTree1 opts (Proxy @f)

instance (Constructor c, GConfigForest1 f) => GConfigTree1 (M1 C c f) where
  gToTree1 opts _ 
    | conIsRecord m = 
      Node 
        (rootOptionsLabelModifier opts . T.pack $ conName m) 
        (gToForest1 (rootOptionsFieldOptions opts) (Proxy @f))
    | otherwise = error "Can only create a tree for named product types i.e. Records with named fields"
    where
      m :: t c f a
      m = undefined
