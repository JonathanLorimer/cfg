{-# LANGUAGE UndecidableInstances #-}

module Cfg.Parser.ConfigParser where

import Cfg.Options (ConfigOptions (..), RootOptions (..))
import Cfg.Parser (ConfigParseError (..), NestedParser (..))
import Data.Kind (Type)
import Data.List
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

defaultParseRootConfig
  :: forall a
   . (Generic a, GRootConfigParser (Rep a))
  => RootOptions
  -> Tree Text
  -> Either ConfigParseError a
defaultParseRootConfig opts tree = fmap to $ gParseRootConfig opts tree

class GRootConfigParser (f :: Type -> Type) where
  gParseRootConfig :: RootOptions -> Tree Text -> Either ConfigParseError (f p)

instance (GRootConfigParser f) => GRootConfigParser (M1 D s f) where
  gParseRootConfig opts tree = M1 <$> gParseRootConfig opts tree

instance (Selector s, GNestedParser f) => GRootConfigParser (M1 S s f) where
  gParseRootConfig opts tree = M1 <$> gParseNestedConfig (rootOptionsFieldOptions opts) tree

instance (Constructor c, GRootConfigParser f) => GRootConfigParser (M1 C c f) where
  gParseRootConfig opts t@(Node label _) =
    if label == (rootOptionsLabelModifier opts . T.pack $ conName m)
      then M1 <$> gParseRootConfig opts t
      else Left $ MismatchedRootKey label (rootOptionsLabelModifier opts . T.pack $ conName m)
   where
    m :: t c f a
    m = undefined

instance (GFieldParser (a :*: b)) => GRootConfigParser (a :*: b) where
  gParseRootConfig opts (Node _ forest) = gParseFields (rootOptionsFieldOptions opts) forest

class FieldParser a where
  parseFields :: [Tree Text] -> Either ConfigParseError a

class GFieldParser (f :: Type -> Type) where
  gParseFields :: ConfigOptions -> [Tree Text] -> Either ConfigParseError (f p)

instance (Selector s, GNestedParser f) => GFieldParser (M1 S s f) where
  gParseFields opts xs = case find ((==) (configOptionsLabelModifier opts . T.pack $ selName @s undefined) . rootLabel) xs of
    Nothing -> Left $ MissingKeys [configOptionsLabelModifier opts . T.pack $ selName @s undefined]
    Just t -> M1 <$> gParseNestedConfig opts t

instance (GFieldParser a, GFieldParser b) => GFieldParser (a :*: b) where
  gParseFields opts xs = do
    a <- gParseFields opts xs
    b <- gParseFields opts xs
    pure $ a :*: b

defaultParseNestedConfig
  :: forall a
   . (Generic a, GNestedParser (Rep a))
  => ConfigOptions
  -> Tree Text
  -> Either ConfigParseError a
defaultParseNestedConfig opts tree = fmap to $ gParseNestedConfig opts tree

class GNestedParser (f :: Type -> Type) where
  gParseNestedConfig :: ConfigOptions -> Tree Text -> Either ConfigParseError (f p)

instance (NestedParser a) => GNestedParser (K1 R a) where
  gParseNestedConfig _ (Node label []) = Left $ MissingValue label
  gParseNestedConfig _ (Node _ [val]) = K1 <$> parseNestedConfig val
  gParseNestedConfig _ tree = K1 <$> parseNestedConfig tree

instance (GNestedParser f) => GNestedParser (M1 D c f) where
  gParseNestedConfig opts t = M1 <$> gParseNestedConfig opts t

instance (Constructor c, GNestedParser f) => GNestedParser (M1 C c f) where
  gParseNestedConfig opts t = M1 <$> gParseNestedConfig opts t

instance (Selector s, GNestedParser f) => GNestedParser (M1 S s f) where
  gParseNestedConfig opts t@(Node label _) =
    if label == modifiedSelectorName
      then Left $ MismatchedKeyAndField label (T.pack $ selName m, modifiedSelectorName)
      else M1 <$> gParseNestedConfig opts t
   where
    m :: t s f a
    m = undefined

    modifiedSelectorName :: Text
    modifiedSelectorName = configOptionsLabelModifier opts . T.pack $ selName m

instance (GFieldParser (a :*: b)) => GNestedParser (a :*: b) where
  gParseNestedConfig opts (Node _ forest) = gParseFields opts forest
