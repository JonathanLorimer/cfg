module Cfg.ParserSpec where

import Cfg.Deriving.Config 
import Cfg.Deriving.Value
import Cfg.Parser
import Data.Text (Text)
import GHC.Generics (Generic (..))
import Test.Hspec
import Data.Map.Strict (fromList, empty)
import KeyTree

data SumTypeConfig = Case1 | Case2
  deriving stock (Generic, Show, Eq)
  deriving (ValueParser) via (Value SumTypeConfig)
  deriving (ConfigParser)

data SubTyCon = SubDataCon
  { subKey1 :: Text
  , subKey2 :: Int
  , subKey3 :: Maybe Bool
  , subKey4 :: Maybe Bool
  }
  deriving (Generic, Show, Eq)
  deriving (ConfigParser) via (Config SubTyCon)

data RootTyCon a = RootDataCon
  { key1 :: SumTypeConfig
  , key2 :: SubTyCon
  , key3 :: Int
  , key4 :: a
  }
  deriving stock (Generic, Show, Eq)
  deriving (ConfigParser) via (Config (RootTyCon a))

spec :: Spec
spec = do
  describe "toConfig" $ do
    it "should parse a type from the sample config" $ do
      let
        subConfig = SubDataCon "Hello World" 27 (Just True) Nothing
      let
        expected :: RootTyCon [Int] = RootDataCon Case1 subConfig 18 [1, 2, 3, 4]
      let
        underTest =
          Free $ fromList
            [ ("key1" , Pure "Case1")
            , ("key2"
              , Free $ fromList
                [ ( "subKey1", Pure "Hello World")
                , ( "subKey2", Pure "27" )
                , ( "subKey3", Pure "True")
                , ( "subKey4", Free empty )
                ]
              )
            , ("key3", Pure "18")
            , ("key4", Pure "[1,2,3,4]")
            ]
      parseConfig underTest `shouldBe` (Right expected)
