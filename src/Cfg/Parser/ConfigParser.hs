{-# LANGUAGE UndecidableInstances #-}
module Cfg.Parser.ConfigParser where

import Cfg.Options (ConfigOptions (..), RootOptions (..))
import Cfg.Parser (ConfigParseError (..), ConfigParser (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics
import Data.List

defaultParseRootConfig ::
    forall a.
    (Generic a, GRootConfigParser (Rep a)) =>
    RootOptions ->
    Tree Text ->
    Either ConfigParseError a
defaultParseRootConfig opts tree = fmap to $ gParseRootConfig opts tree

class GRootConfigParser (f :: Type -> Type) where
    gParseRootConfig :: RootOptions -> Tree Text -> Either ConfigParseError (f p)

instance GRootConfigParser f => GRootConfigParser (M1 D s f) where
    gParseRootConfig opts tree = M1 <$> gParseRootConfig opts tree

instance (Selector s, GConfigParser f) => GRootConfigParser (M1 S s f) where
    gParseRootConfig opts tree = M1 <$> gParseConfig (rootOptionsFieldOptions opts) tree

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

instance (Selector s, GConfigParser f) => GFieldParser (M1 S s f) where
    gParseFields opts xs = case find ((==) (configOptionsLabelModifier opts . T.pack $ selName @s undefined) . rootLabel) xs of
                              Nothing -> Left $ MissingKeys [configOptionsLabelModifier opts . T.pack $ selName @s undefined]
                              Just t -> M1 <$> gParseConfig opts t

instance (GFieldParser a, GFieldParser b) => GFieldParser (a :*: b) where
    gParseFields opts xs = do
        a <- gParseFields opts xs
        b <- gParseFields opts xs
        pure $ a :*: b

defaultParseConfig ::
    forall a.
    (Generic a, GConfigParser (Rep a)) =>
    ConfigOptions ->
    Tree Text ->
    Either ConfigParseError a
defaultParseConfig opts tree = fmap to $ gParseConfig opts tree

class GConfigParser (f :: Type -> Type) where
    gParseConfig :: ConfigOptions -> Tree Text -> Either ConfigParseError (f p)

instance ConfigParser a => GConfigParser (K1 R a) where
    gParseConfig _ (Node label []) = Left $ MissingValue label
    gParseConfig _ (Node _ [val]) = K1 <$> parseConfig val
    gParseConfig _ tree = K1 <$> parseConfig tree

instance (GConfigParser f) => GConfigParser (M1 D c f) where
    gParseConfig opts t = M1 <$> gParseConfig opts t

instance (Constructor c, GConfigParser f) => GConfigParser (M1 C c f) where
    gParseConfig opts t = M1 <$> gParseConfig opts t

instance (Selector s, GConfigParser f) => GConfigParser (M1 S s f) where
    gParseConfig opts t@(Node label _) =
        if label == modifiedSelectorName
            then Left $ MismatchedKeyAndField label (T.pack $ selName m, modifiedSelectorName)
            else M1 <$> gParseConfig opts t
      where
        m :: t s f a
        m = undefined

        modifiedSelectorName :: Text
        modifiedSelectorName = configOptionsLabelModifier opts . T.pack $ selName m

instance (GFieldParser (a :*: b)) => GConfigParser (a :*: b) where
    gParseConfig opts (Node _ forest) = gParseFields opts forest
