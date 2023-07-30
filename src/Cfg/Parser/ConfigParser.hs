{-# LANGUAGE UndecidableInstances #-}

module Cfg.Parser.ConfigParser where

import Cfg.Options 
import Cfg.Parser (ConfigParseError (..), ConfigParser(..))
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics
import KeyTree
import Data.Map qualified as M

defaultParseConfig
  :: forall a
   . (Generic a, GConfigParser (Rep a))
  => ConfigOptions
  -> KeyTree Text Text
  -> Either ConfigParseError a
defaultParseConfig opts tree = fmap to $ gParseConfig opts tree

class GConfigParser (f :: Type -> Type) where
  gParseConfig :: ConfigOptions -> KeyTree Text Text -> Either ConfigParseError (f p)

instance (ConfigParser a) => GConfigParser (K1 R a) where
  gParseConfig _ kt = K1 <$> parseConfig kt

instance (GConfigParser f, Datatype d) => GConfigParser (M1 D d f) where
  gParseConfig opts t@(Free keyForest) = 
    case opts of
      Root (RootOptions { rootOptionsRootKey = TyCon modifier }) -> 
        let key = modifier . T.pack $ datatypeName @d undefined
         in case M.lookup key keyForest of
              Just subTree -> M1 <$> gParseConfig opts subTree
              Nothing -> Left $ MissingKeys [key] t
      _ -> M1 <$> gParseConfig opts t
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
    where
      key = keyModifier opts . T.pack $ datatypeName @d undefined

instance (Constructor c, GConfigParser f) => GConfigParser (M1 C c f) where
  gParseConfig opts t@(Free keyForest) = 
    case opts of
      Root (RootOptions { rootOptionsRootKey = DataCon modifier }) -> 
        let key = modifier . T.pack $ conName @c undefined
         in case M.lookup key keyForest of
              Just subTree -> M1 <$> gParseConfig opts subTree
              Nothing -> Left $ MissingKeys [key] t
      _ -> M1 <$> gParseConfig opts t
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
    where
      key = keyModifier opts . T.pack $ conName @c undefined


instance (Selector s, GConfigParser f) => GConfigParser (M1 S s f) where
  gParseConfig opts (Pure value) = Left $ ExpectedKeyFoundValue key value
    where
      key = keyModifier opts . T.pack $ selName @s undefined
  gParseConfig opts t@(Free keyForest) =
    case M.lookup selectorName keyForest of
      Nothing -> Left $ MissingKeys [selectorName] t 
      Just subTree -> M1 <$> gParseConfig opts subTree
     where
       selectorName = keyModifier opts . T.pack $ selName @s undefined

instance (GConfigParser a, GConfigParser b) => GConfigParser (a :*: b) where
  gParseConfig opts xs = do
    a <- gParseConfig opts xs
    b <- gParseConfig opts xs
    pure $ a :*: b

-- class FieldParser a where
--   parseFields :: KeyForest Text Text -> Either ConfigParseError a
--
-- class GFieldParser (f :: Type -> Type) where
--   gParseFields :: ConfigOptions -> KeyForest Text Text -> Either ConfigParseError (f p)
--
-- instance (Selector s, GNestedParser f) => GFieldParser (M1 S s f) where
--   gParseFields opts keyForest = 
--     case M.lookup selectorName keyForest of
--       Nothing -> Left $ MissingKeys [selectorName] (Free keyForest)
--       Just t -> M1 <$> gParseNestedConfig opts t
--      where
--        selectorName = configOptionsLabelModifier opts . T.pack $ selName @s undefined
--
-- instance (GFieldParser a, GFieldParser b) => GFieldParser (a :*: b) where
--   gParseFields opts xs = do
--     a <- gParseFields opts xs
--     b <- gParseFields opts xs
--     pure $ a :*: b
--
-- defaultParseNestedConfig
--   :: forall a
--    . (Generic a, GNestedParser (Rep a))
--   => ConfigOptions
--   -> KeyTree Text Text
--   -> Either ConfigParseError a
-- defaultParseNestedConfig opts tree = fmap to $ gParseNestedConfig opts tree
--
-- class GNestedParser (f :: Type -> Type) where
--   gParseNestedConfig :: ConfigOptions -> KeyTree Text Text -> Either ConfigParseError (f p)
--
-- instance (NestedParser a) => GNestedParser (K1 R a) where
--   gParseNestedConfig _ kt = K1 <$> parseNestedConfig kt
--
-- instance (GNestedParser f) => GNestedParser (M1 D c f) where
--   gParseNestedConfig opts t = M1 <$> gParseNestedConfig opts t
--
-- instance (Constructor c, GNestedParser f) => GNestedParser (M1 C c f) where
--   gParseNestedConfig opts t = M1 <$> gParseNestedConfig opts t
--
-- instance (Selector s, GNestedParser f) => GNestedParser (M1 S s f) where
--   gParseNestedConfig opts t@(Node label _) =
--     if label == modifiedSelectorName
--       then Left $ MismatchedKeyAndField label (T.pack $ selName m, modifiedSelectorName)
--       else M1 <$> gParseNestedConfig opts t
--    where
--     m :: t s f a
--     m = undefined
--
--     modifiedSelectorName :: Text
--     modifiedSelectorName = configOptionsLabelModifier opts . T.pack $ selName m
--
--     case M.lookup selectorName keyForest of
--       Nothing -> Left $ MissingKeys [selectorName] (Free keyForest)
--       Just t -> M1 <$> gParseNestedConfig opts t
--      where
--        selectorName = configOptionsLabelModifier opts . T.pack $ selName @s undefined
--
-- instance (GFieldParser (a :*: b)) => GNestedParser (a :*: b) where
--   gParseNestedConfig opts (Node _ forest) = gParseFields opts forest
