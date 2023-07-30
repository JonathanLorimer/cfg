module Cfg.SourceSpec where

import Cfg.Deriving.ConfigRoot
import Cfg.Deriving.ConfigValue
import Cfg.Deriving.LabelModifier
import Cfg.Deriving.SubConfig
import Cfg.Source
import Data.Text (Text)
import KeyTree
import GHC.Generics (Generic (..))
import Test.Hspec
import Data.Map.Strict (fromList, empty)

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
    -- it "should create a tree with modified options from sample config" $ do
    --   let
    --     expected =
    --       Node
    --         "ROOTDATACONOPTS"
    --         [ Node "keyopts1" []
    --         , Node
    --             "keyopts2"
    --             [ Node "SUBKEY1" []
    --             , Node "SUBKEY2" []
    --             , Node "SUBKEY3" []
    --             ]
    --         , Node "keyopts3" []
    --         , Node "keyopts4" []
    --         ]
    --   configSource @(RootTyConOpts Text) `shouldBe` expected

data SumTypeConfig = Case1 | Case2
  deriving stock (Generic, Show)
  deriving (ConfigSource) via ConfigValue SumTypeConfig

data SubTyCon = SubDataCon
  { subKey1 :: Text
  , subKey2 :: Int
  , subKey3 :: Maybe Bool
  }
  deriving (Generic, Show)
  deriving (ConfigSource) via (Config SubTyCon)

data RootTyCon a = RootDataCon
  { key1 :: SumTypeConfig
  , key2 :: SubTyCon
  , key3 :: Int
  , key4 :: a
  }
  deriving stock (Generic, Show)
  deriving (ConfigSource) via (Config (RootTyCon a))

-- data SubTyConOpts = SubDataConOpts
--   { subKeyOpts1 :: Text
--   , subKeyOpts2 :: Int
--   , subKeyOpts3 :: Maybe Bool
--   }
--   deriving (Generic, Show)
--   deriving (ConfigSource) via (SubConfigOpts ToUpper SubTyCon)
--
-- data RootTyConOpts a = RootDataConOpts
--   { keyOpts1 :: SumTypeConfig
--   , keyOpts2 :: SubTyConOpts
--   , keyOpts3 :: Int
--   , keyOpts4 :: a
--   }
--   deriving stock (Generic, Show)
--   deriving (ConfigSource) via (ConfigRootOpts ToUpper ToLower (RootTyConOpts a))
