module Cfg.Deriving.LabelModifierSpec where

import Cfg.Deriving.LabelModifier
import Test.Hspec

spec :: Spec
spec = do
  describe "basic modifiers" $ do
    it "ToLower" $ do
      let
        text = "ThisShouldBeLower1234"
      getLabelModifier @ToLower text `shouldBe` "thisshouldbelower1234"
    it "ToUpper" $ do
      let
        text = "ThisShouldBeUpper1234"
      getLabelModifier @ToUpper text `shouldBe` "THISSHOULDBEUPPER1234"
    it "LowerFirst" $ do
      let
        text = "ThisShouldBeLower1234"
      getLabelModifier @LowerFirst text `shouldBe` "thisShouldBeLower1234"
    it "UpperFirst" $ do
      let
        text = "thisShouldBeLower1234"
      getLabelModifier @UpperFirst text `shouldBe` "ThisShouldBeLower1234"
    it "StripPrefix" $ do
      let
        text = "someConvalutedRecordNameRecordValue"
      getLabelModifier @(StripPrefix "someConvalutedRecordName") text
        `shouldBe` "RecordValue"
    it "StripSuffix" $ do
      let
        text = "superCoolApplicationConfig"
      getLabelModifier @(StripSuffix "Config") text
        `shouldBe` "superCoolApplication"
    it "CamelToSnake" $ do
      let
        text = "superCoolApplicationConfig"
      getLabelModifier @CamelToSnake text
        `shouldBe` "super_cool_application_config"
    it "CamelToKebab" $ do
      let
        text = "superCoolApplicationConfig"
      getLabelModifier @CamelToKebab text
        `shouldBe` "super-cool-application-config"
    it "should work as a list, applied in left to right order" $ do
      let
        text = "superCoolApplicationConfig"
      getLabelModifier @[StripSuffix "Config", CamelToSnake, ToUpper] text
        `shouldBe` "SUPER_COOL_APPLICATION"
    it "should work as a tuple, applied in left to right order" $ do
      let
        text = "superCoolApplicationConfig"
      getLabelModifier @(StripSuffix "Config", CamelToSnake, ToUpper) text
        `shouldBe` "SUPER_COOL_APPLICATION"
