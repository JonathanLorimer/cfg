module Cfg.ParserSpec where

import Cfg.Deriving.ConfigRoot (ConfigRoot)
import Cfg.Deriving.SubConfig (SubConfig)
import Cfg.Source (ConfigValue, NestedConfig, RootConfig (..))
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Tree (Tree (..))
import GHC.Generics (Generic (..))
import Test.Hspec

data SumTypeConfig = Case1 | Case2
    deriving stock (Generic, Show)
    deriving (NestedConfig) via ConfigValue SumTypeConfig

data SubTyCon = SubDataCon
    { subKey1 :: Text
    , subKey2 :: Int
    , subKey3 :: Maybe Bool
    }
    deriving (Generic, Show)
    deriving (NestedConfig) via (SubConfig SubTyCon)

data RootTyCon a = RootDataCon
    { key1 :: SumTypeConfig
    , key2 :: SubTyCon
    , key3 :: Int
    , key4 :: a
    }
    deriving stock (Generic, Show)
    deriving (RootConfig) via (ConfigRoot (RootTyCon a))

spec :: Spec
spec = do
    describe "toRootConfig" $ do
        it "should parse a tree from the sample config" $ do
            let expected =
                    Node
                        "RootDataCon"
                        [ Node "key1" []
                        , Node
                            "key2"
                            [ Node "subKey1" []
                            , Node "subKey2" []
                            , Node "subKey3" []
                            ]
                        , Node "key3" []
                        , Node "key4" []
                        ]
            toRootConfig (Proxy :: Proxy (RootTyCon Text)) `shouldBe` expected

--
-- data SubTyConOpts =
--   SubDataConOpts
--     { subKeyOpts1 :: Text
--     , subKeyOpts2 :: Int
--     , subKeyOpts3 :: Maybe Bool
--     }
--     deriving (Generic, Show)
--     deriving NestedConfig via (SubConfigOpts ToUpper SubTyCon)
--
-- data RootTyConOpts a =
--   RootDataConOpts
--     { keyOpts1 :: SumTypeConfig
--     , keyOpts2 :: SubTyConOpts
--     , keyOpts3 :: Int
--     , keyOpts4 :: a
--     }
--     deriving stock (Generic, Show)
--     deriving RootConfig via (ConfigRootOpts ToUpper ToLower (RootTyConOpts a))
