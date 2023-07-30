module Cfg.SourceSpec where

import Cfg.Deriving.ConfigRoot
import Cfg.Deriving.ConfigValue
import Cfg.Deriving.LabelModifier
import Cfg.Deriving.SubConfig
import Cfg.Source
import Data.Text (Text)
import Data.Tree (Tree (..))
import GHC.Generics (Generic (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "" $ do
    it "" $ do
      True `shouldBe` True

--   describe "toRootConfig" $ do
--     it "should create a tree from the sample config" $ do
--       let
--         expected =
--           Node
--             "RootDataCon"
--             [ Node "key1" []
--             , Node
--                 "key2"
--                 [ Node "subKey1" []
--                 , Node "subKey2" []
--                 , Node "subKey3" []
--                 ]
--             , Node "key3" []
--             , Node "key4" []
--             ]
--       toRootConfig @(RootTyCon Text) `shouldBe` expected
--     it "should create a tree with modified options from sample config" $ do
--       let
--         expected =
--           Node
--             "ROOTDATACONOPTS"
--             [ Node "keyopts1" []
--             , Node
--                 "keyopts2"
--                 [ Node "SUBKEY1" []
--                 , Node "SUBKEY2" []
--                 , Node "SUBKEY3" []
--                 ]
--             , Node "keyopts3" []
--             , Node "keyopts4" []
--             ]
--       toRootConfig @(RootTyConOpts Text) `shouldBe` expected
--
-- data SumTypeConfig = Case1 | Case2
--   deriving stock (Generic, Show)
--   deriving (NestedConfig) via ConfigValue SumTypeConfig
--
-- data SubTyCon = SubDataCon
--   { subKey1 :: Text
--   , subKey2 :: Int
--   , subKey3 :: Maybe Bool
--   }
--   deriving (Generic, Show)
--   deriving (NestedConfig) via (SubConfig SubTyCon)
--
-- data RootTyCon a = RootDataCon
--   { key1 :: SumTypeConfig
--   , key2 :: SubTyCon
--   , key3 :: Int
--   , key4 :: a
--   }
--   deriving stock (Generic, Show)
--   deriving (RootConfig) via (ConfigRoot (RootTyCon a))
--
-- data SubTyConOpts = SubDataConOpts
--   { subKeyOpts1 :: Text
--   , subKeyOpts2 :: Int
--   , subKeyOpts3 :: Maybe Bool
--   }
--   deriving (Generic, Show)
--   deriving (NestedConfig) via (SubConfigOpts ToUpper SubTyCon)
--
-- data RootTyConOpts a = RootDataConOpts
--   { keyOpts1 :: SumTypeConfig
--   , keyOpts2 :: SubTyConOpts
--   , keyOpts3 :: Int
--   , keyOpts4 :: a
--   }
--   deriving stock (Generic, Show)
--   deriving (RootConfig) via (ConfigRootOpts ToUpper ToLower (RootTyConOpts a))
