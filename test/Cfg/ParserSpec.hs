module Cfg.ParserSpec where

import Cfg.Deriving.ConfigRoot (ConfigRoot (..))
import Cfg.Deriving.ConfigValue
import Cfg.Deriving.SubConfig (SubConfig (..))
import Cfg.Parser
import Data.Text (Text)
import Data.Tree (Tree (..))
import GHC.Generics (Generic (..))
import Test.Hspec

data SumTypeConfig = Case1 | Case2
  deriving stock (Generic, Show, Eq)
  deriving (ValueParser) via (ConfigValue SumTypeConfig)
  deriving (ConfigParser)

data SubTyCon = SubDataCon
  { subKey1 :: Text
  , subKey2 :: Int
  , subKey3 :: Maybe Bool
  }
  deriving (Generic, Show, Eq)
  deriving (ConfigParser) via (SubConfig SubTyCon)

data RootTyCon a = RootDataCon
  { key1 :: SumTypeConfig
  , key2 :: SubTyCon
  , key3 :: Int
  , key4 :: a
  }
  deriving stock (Generic, Show, Eq)
  deriving (RootParser) via (ConfigRoot (RootTyCon a))

spec :: Spec
spec = do
  describe "toRootConfig" $ do
    it "should parse a type from the sample config" $ do
      let
        subConfig = SubDataCon "Hello World" 27 (Just True)
      let
        expected :: RootTyCon [Int] = RootDataCon Case1 subConfig 18 [1, 2, 3, 4]
      let
        underTest =
          Node
            "RootDataCon"
            [ Node "key1" [Node "Case1" []]
            , Node
                "key2"
                [ Node "subKey1" [Node "Hello World" []]
                , Node "subKey2" [Node "27" []]
                , Node "subKey3" [Node "Just True" []]
                ]
            , Node "key3" [Node "18" []]
            , Node "key4" [Node "[1,2,3,4]" []]
            ]
      parseRootConfig underTest `shouldBe` (Right expected)
