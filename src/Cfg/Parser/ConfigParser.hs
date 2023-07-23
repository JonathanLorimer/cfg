{-# LANGUAGE UndecidableInstances #-}

module Cfg.Parser.ConfigParser where

import Cfg.Options (ConfigOptions (..), RootOptions (..))
import Cfg.Parser (ConfigParseError (..), ConfigParser (..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tree (Tree (..))
import GHC.Generics

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

instance (Constructor c, GFieldParser f) => GRootConfigParser (M1 C c f) where
    gParseRootConfig opts (Node label forest) =
        if label == (rootOptionsLabelModifier opts . T.pack $ conName m)
            then M1 <$> gParseFields (rootOptionsFieldOptions opts) forest
            else Left $ MismatchedRootKey label (rootOptionsLabelModifier opts . T.pack $ conName m)
      where
        m :: t c f a
        m = undefined

class FieldParser a where
    parseFields :: [Tree Text] -> Either ConfigParseError a

class GFieldParser (f :: Type -> Type) where
    gParseFields :: ConfigOptions -> [Tree Text] -> Either ConfigParseError (f p)

-- NOTE: This is undecidable, but is really just the base case for products and we want to hand off
-- dealing with field selectors to the ConfigParser class
instance (GConfigParser (M1 S s f)) => GFieldParser (M1 S s f) where
    gParseFields _ [] = Left $ MissingKeys []
    gParseFields opts [t] = gParseConfig opts t
    gParseFields _ xs = Left $ UnmatchedFields xs

-- NOTE: It seems like we are depending on the order of fieldSelectors corresponding with the order of our tree.
-- Since they are generated from the same type this is probably fine, but might case some issues
instance (GConfigParser a, GFieldParser b) => GFieldParser (a :*: b) where
    gParseFields _ [] = Left $ MissingKeys []
    gParseFields _ (_ : []) = Left $ MissingKeys []
    gParseFields opts (x : xs) = do
        a <- gParseConfig opts x
        b <- gParseFields opts xs
        pure $ a :*: b

-- TODO: Build up product type by traversing the list and replacing cons with :*:
-- Then recurse into field vals using `GConfigParser`

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
    gParseConfig _ t = K1 <$> parseConfig t

instance (Constructor c, GConfigParser f) => GConfigParser (M1 D c f) where
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

data ExampleSubTy = ExampleSubDat
    { subField1 :: Text
    , subField2 :: Int
    }
    deriving (Generic, Show)

data ExampleTy = ExampleDat
    { field1 :: Text
    , field2 :: ExampleSubTy
    }
    deriving (Generic, Show)

-- subSample = (ExampleSubDat "hello" 24)
--
-- sample = ExampleDat "hello" subSample
--
-- -- $> import Cfg.Parser.ConfigParser
-- --
-- -- $> import Cfg.Parser
-- --
-- -- $> import GHC.Generics
-- --
-- -- $> from sample
-- --
-- -- $> :t (from sample)
-- --
-- -- $> :t (from subSample)
