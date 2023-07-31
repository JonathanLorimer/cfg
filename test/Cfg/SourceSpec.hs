module Cfg.SourceSpec where

import Cfg.Deriving.Config
import Cfg.Deriving.Value
import Cfg.Deriving.KeyModifier
import Cfg.Source
import Data.Text (Text)
import KeyTree
import GHC.Generics (Generic (..))
import Test.Hspec
import Data.Map.Strict (fromList, empty, singleton)
import Cfg.Options
import Cfg.Source.Default

spec :: Spec
spec = do
  describe "configSource" $ do
    it "should create a tree from the sample config" $ do
      let
        expected =
          Free $ fromList
            [ ("key1", Free empty)
            , ("key2", Free $ fromList
                [ ("subKey1", Free empty)
                , ("subKey2", Free empty)
                , ("subKey3", Free empty)
                ]
              )
            , ("key3", Free empty)
            , ("key4", Free empty)
            ]
      configSource @(RootTyCon Text) `shouldBe` expected
    it "should create a tree with modified options from sample config" $ do
      let
        expected =
          Free $ singleton "ROOTTYCONOPTS" $
          Free $ fromList
              [ ("keyopts1", Free empty)
              , ("keyopts2", Free $ fromList
                  [ ("SUBKEYOPTS1", Free empty)
                  , ("SUBKEYOPTS2", Free empty)
                  , ("SUBKEYOPTS3", Free empty)
                  ]
                )
              , ("keyopts3", Free empty)
              , ("keyopts4", Free empty)
              ]
      configSource @(RootTyConOpts Text) `shouldBe` expected
    it "should create a tree with modified options from sample config AND respect defaults" $ do
      let
        expected =
          Free $ singleton "ROOTTYCONOPTSDEF" $
          Free $ fromList
              [ ("keyoptsdef1", Pure "Case1")
              , ("keyoptsdef2", Free $ fromList
                  [ ("SUBKEYOPTS1", Free empty)
                  , ("SUBKEYOPTS2", Free empty)
                  , ("SUBKEYOPTS3", Free empty)
                  ]
                )
              , ("keyoptsdef3", Free empty)
              , ("keyoptsdef4", Free empty)
              ]
      configSource @(RootTyConOptsDef Text) `shouldBe` expected

data SumTypeConfig = Case1 | Case2
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource) via Value SumTypeConfig

data SubTyCon = SubDataCon
  { subKey1 :: Text
  , subKey2 :: Int
  , subKey3 :: Maybe Bool
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource) via (Config SubTyCon)

data RootTyCon a = RootDataCon
  { key1 :: SumTypeConfig
  , key2 :: SubTyCon
  , key3 :: Int
  , key4 :: a
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource) via (Config (RootTyCon a))

data SubTyConOpts = SubDataConOpts
  { subKeyOpts1 :: Text
  , subKeyOpts2 :: Int
  , subKeyOpts3 :: Maybe Bool
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource) via (ConfigOpts ToUpper SubTyConOpts)

data RootTyConOpts a = RootDataConOpts
  { keyOpts1 :: SumTypeConfig
  , keyOpts2 :: SubTyConOpts
  , keyOpts3 :: Int
  , keyOpts4 :: a
  }
  deriving (Generic, Show, DefaultSource)
  deriving (ConfigSource) via (ConfigRoot ('TypeName ToUpper) ToLower (RootTyConOpts a))

data RootTyConOptsDef a = RootTyConOptsDef
  { keyOptsDef1 :: SumTypeConfig
  , keyOptsDef2 :: SubTyConOpts
  , keyOptsDef3 :: Int
  , keyOptsDef4 :: a
  }
  deriving (Generic, Show)
  deriving (ConfigSource) via (ConfigRoot ('TypeName ToUpper) ToLower (RootTyConOptsDef a))

instance DefaultSource (RootTyConOptsDef a) where
  defaults "keyOptsDef1" = Just "Case1"
  defaults _ = Nothing
