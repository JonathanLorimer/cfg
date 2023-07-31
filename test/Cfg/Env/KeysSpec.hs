module Cfg.Env.KeysSpec where

import Data.Text (Text)
import Test.Hspec
import KeyTree
import Data.Map.Strict (fromList, empty, singleton)
import Cfg.Env.Keys
import Cfg.Source (ConfigSource)
import Cfg.Options
import Cfg.Deriving.KeyModifier
import GHC.Generics
import Cfg.Deriving

spec :: Spec
spec = do
  describe "getKeys" $ do
    it "should collect all tree keys" $ do
      let
        tree :: KeyTree Text Text = 
          Free $ fromList
            [ ("A" , Pure "Case1")
            , ("B"
              , Free $ fromList
                [ ( "1", Pure "Hello World")
                , ( "2", Pure "27" )
                , ( "3", Pure "True")
                , ( "4", Free empty )
                ]
              )
            , ("C", Pure "18")
            , ("D", 
                Free $ singleton "E" $
                  Free $ singleton "F" $
                    Free $ singleton "G" $
                      Free $ singleton "H" $
                        Free $ singleton "I" $
                          Free $ singleton "J" $
                            Free $ singleton "K" $
                              Pure "[1,2,3,4]"
              )
            ]
      let expected = 
            [ ["A"]
            , ["B", "1"]
            , ["B", "2"]
            , ["B", "3"]
            , ["B", "4"]
            , ["C"]
            , ["D", "E", "F", "G", "H", "I", "J", "K"]
            ]
      getKeys tree `shouldBe` expected
  describe "showEnvKeys" $ do
    it "should concatenate all tree keys with separator" $ do
      let expected = 
            [ "ROOT_DATA_CON_OPTS__KEY_OPTS1"
            , "ROOT_DATA_CON_OPTS__KEY_OPTS2__SUB_KEY_OPTS1"
            , "ROOT_DATA_CON_OPTS__KEY_OPTS2__SUB_KEY_OPTS2"
            , "ROOT_DATA_CON_OPTS__KEY_OPTS2__SUB_KEY_OPTS3"
            , "ROOT_DATA_CON_OPTS__KEY_OPTS3"
            , "ROOT_DATA_CON_OPTS__KEY_OPTS4"
            ]
      showEnvKeys @(RootTyConOpts Text) "__"  `shouldBe` expected

data SumTypeConfig = Case1 | Case2
  deriving stock (Generic, Show)
  deriving (ConfigSource) via Value SumTypeConfig

data SubTyConOpts = SubDataConOpts
  { subKeyOpts1 :: Text
  , subKeyOpts2 :: Int
  , subKeyOpts3 :: Maybe Bool
  }
  deriving (Generic, Show)
  deriving (ConfigSource) via (ConfigOpts [CamelToSnake, ToUpper] SubTyConOpts)

data RootTyConOpts a = RootDataConOpts
  { keyOpts1 :: SumTypeConfig
  , keyOpts2 :: SubTyConOpts
  , keyOpts3 :: Int
  , keyOpts4 :: a
  }
  deriving stock (Generic, Show)
  deriving (ConfigSource) via (ConfigRoot ('ConstructorName [CamelToSnake, ToUpper]) [CamelToSnake, ToUpper] (RootTyConOpts a))
