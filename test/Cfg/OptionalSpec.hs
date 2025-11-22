module Cfg.OptionalSpec where

import Cfg.Deriving.Config
import Cfg.Parser
import Data.Map.Strict (empty, fromList)
import Data.Text (Text)
import GHC.Generics (Generic (..))
import KeyTree
import Test.Hspec
import Cfg.Source (ConfigSource (..))
import Cfg.Optional (OptionalConfig (..))
import Cfg.Source.Default (DefaultSource)

data SubSubTyCon = SubSubDataCon
  { subSubKey1 :: Text
  , subSubKey2 :: Int
  }
  deriving (Generic, Show, Eq, DefaultSource)
  deriving (ConfigParser, ConfigSource) via (Config SubSubTyCon)

data SubTyCon = SubDataCon
  { subKey2 :: Int
  , subKey1 :: OptionalConfig SubSubTyCon
  }
  deriving (Generic, Show, Eq, DefaultSource)
  deriving (ConfigParser, ConfigSource) via (Config SubTyCon)

data RootTyCon = RootDataCon
  { key1 :: Bool
  , key2 :: SubTyCon
  }
  deriving (Generic, Show, Eq, DefaultSource)
  deriving (ConfigParser, ConfigSource) via (Config RootTyCon)

spec :: Spec
spec = do
  describe "configParser" $ do
    it "should parse a type from the sample config containing a present optional config" $ do
      let
        subSubConfig = SubSubDataCon "Hello World" 27
        subConfig = SubDataCon 64 (OptionalConfig $ Just subSubConfig)
        expected = RootDataCon True subConfig
        underTest =
          Free $
            fromList
              [ ("key1", Pure "True")
              ,
                ( "key2"
                , Free $
                    fromList
                      [ ("subKey1"
                        , Free $ fromList
                          [ ("subSubKey1", Pure "Hello World")
                          , ("subSubKey2", Pure "27")
                          ]
                        )
                      , ("subKey2", Pure "64")
                      ]
                )
              ]
      parseConfig underTest `shouldBe` Right expected
    it "should parse a type from the sample config with a missing optional config" $ do
      let
        subConfig = SubDataCon 64 (OptionalConfig Nothing)
        expected = RootDataCon True subConfig
        underTest =
          Free $
            fromList
              [ ("key1", Pure "True")
              , ("key2"
                , Free $ fromList
                    [ ("subKey2", Pure "64")
                    , ("subKey1", Free empty)
                    ]
                )
              ]
      parseConfig underTest `shouldBe` Right expected
    it "should parse Nothing when optional config has key stubs but no values (env source style)" $ do
      let
        subConfig = SubDataCon 64 (OptionalConfig Nothing)
        expected = RootDataCon True subConfig
        underTest =
          Free $
            fromList
              [ ("key1", Pure "True")
              , ("key2"
                , Free $ fromList
                    [ ("subKey2", Pure "64")
                    , ("subKey1"
                      , Free $ fromList
                          [ ("subSubKey1", Free empty)
                          , ("subSubKey2", Free empty)
                          ]
                      )
                    ]
                )
              ]
      parseConfig underTest `shouldBe` Right expected
  describe "configSource" $ do
    it "should create a tree from the sample config" $ do
      let
        expected =
          Free $
            fromList
              [ ("key1", Free empty)
              ,
                ( "key2"
                , Free $
                    fromList
                      [ ("subKey1"
                        , Free $ fromList
                          [ ("subSubKey1", Free empty)
                          , ("subSubKey2", Free empty)
                          ]
                        )
                      , ("subKey2", Free empty)
                      ]
                )
              ]
      configSource @RootTyCon `shouldBe` expected
