module Cfg.SourceSpec where

import Test.Hspec
import Data.Tree (Tree(..))
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Data.Data (Proxy(..))
import Cfg.Source (RootConfig(..))
import Cfg.Source.Deriving (ConfigRoot, SubConfig)
import Cfg.Source (NestedConfig)
import Cfg.Source.Deriving (ToUpper)
import Cfg.Source.Deriving (SubConfigOpts)
import Cfg.Source.Deriving (ConfigRootOpts)
import Cfg.Source (ConfigValue)
import Cfg.Source.Deriving (ToLower)


spec :: Spec
spec = do
  describe "toRootConfig" $ do
    it "should create a tree from the sample config" $ do
      let expected = 
            Node "RootDataCon"
              [ Node "key1" []
              , Node "key2"
                [ Node "subKey1" []
                , Node "subKey2" []
                , Node "subKey3" []
                ]
              ,Node "key3" []
              ,Node "key4" []
              ]
      toRootConfig (Proxy :: Proxy (RootTyCon Text)) `shouldBe` expected
    it "should create a tree with modified options from sample config" $ do
      let expected = 
            Node "ROOTDATACONOPTS"
              [ Node "keyopts1" []
              , Node "keyopts2"
                [ Node "SUBKEY1" []
                , Node "SUBKEY2" []
                , Node "SUBKEY3" []
                ]
              ,Node "keyopts3" []
              ,Node "keyopts4" []
              ]
      toRootConfig (Proxy :: Proxy (RootTyConOpts Text)) `shouldBe` expected

data SumTypeConfig = Case1 | Case2 
    deriving stock (Generic, Show)
    deriving NestedConfig via ConfigValue SumTypeConfig

data SubTyCon = 
  SubDataCon
    { subKey1 :: Text
    , subKey2 :: Int
    , subKey3 :: Maybe Bool
    } 
    deriving (Generic, Show)
    deriving NestedConfig via (SubConfig SubTyCon)

data RootTyCon a = 
  RootDataCon
    { key1 :: SumTypeConfig
    , key2 :: SubTyCon
    , key3 :: Int
    , key4 :: a
    } 
    deriving stock (Generic, Show)
    deriving RootConfig via (ConfigRoot (RootTyCon a))

data SubTyConOpts = 
  SubDataConOpts
    { subKeyOpts1 :: Text
    , subKeyOpts2 :: Int
    , subKeyOpts3 :: Maybe Bool
    } 
    deriving (Generic, Show)
    deriving NestedConfig via (SubConfigOpts ToUpper SubTyCon)

data RootTyConOpts a = 
  RootDataConOpts
    { keyOpts1 :: SumTypeConfig
    , keyOpts2 :: SubTyConOpts
    , keyOpts3 :: Int
    , keyOpts4 :: a
    } 
    deriving stock (Generic, Show)
    deriving RootConfig via (ConfigRootOpts ToUpper ToLower (RootTyConOpts a))
