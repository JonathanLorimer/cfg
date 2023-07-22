module Cfg.SourceSpec where

import Test.Hspec
import Data.Tree (Tree(..))
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Data.Data (Proxy(..))
import Cfg.Source (RootConfig(..))
import Cfg.Source.Deriving (ConfigRoot, SubConfig)
import Cfg.Source (NestedConfig)

data SumTypeConfig = Case1 | Case2 
    deriving stock (Generic, Show)
    deriving NestedConfig via (SubConfig SumTypeConfig)

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


spec :: Spec
spec = do
  describe "defaultToTree" $ do
    it "should create a tree from the sample config" $ do
      let subConfig = SubDataCon "hello" 24 (Just True)
      print $ from subConfig
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
