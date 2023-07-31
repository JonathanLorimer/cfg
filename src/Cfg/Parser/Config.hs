{-# LANGUAGE UndecidableInstances #-}

module Cfg.Parser.Config where

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
      Root (RootOptions { rootOptionsRootKey = TypeName modifier }) -> 
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
      Root (RootOptions { rootOptionsRootKey = ConstructorName modifier }) -> 
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
