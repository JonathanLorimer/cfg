module Cfg.Source.GenericSpec where

import Test.Hspec
import Data.Tree (Tree(..))
import Data.Text (Text)
import GHC.Generics (Generic (..))
-- import Cfg.Source.Generic (defaultToTree)
import Data.Data (Proxy(..))
import Cfg.Source (ConfigTree(..), defaultToTree)

data SumTypeConfig = Case1 | Case2 deriving (Generic, Show)

data SubConfig = 
  SubConfig
    { subKey1 :: Text
    , subKey2 :: Int
    , subKey3 :: Maybe Bool
    } deriving (Generic, Show)

data RootConfig = 
  RootConfig
    { key1 :: SumTypeConfig
    , key2 :: SubConfig
    , key3 :: Int
    -- , key4 :: a
    } deriving (Generic, Show)

spec :: Spec
spec = do
  describe "defaultToTree" $ do
    it "should create a tree from the sample config" $ do
      let subConfig = SubConfig "hello" 24 (Just True)
      print $ from subConfig
      let expected = 
            Node "RootConfig"
              [ Node "key1" []
              , Node "key2"
                [ Node "subKey1" []
                , Node "subKey2" []
                , Node "subKey3" []
                ]
              ,Node "key3" []
              ,Node "key4" []
              ]
      defaultToTree (Proxy :: Proxy RootConfig) `shouldBe` expected
