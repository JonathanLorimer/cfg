module Cfg.Deriving.KeyModifierSpec where

import Cfg.Deriving.KeyModifier
import Test.Hspec

spec :: Spec
spec = do
  describe "basic modifiers" $ do
    it "ToLower" $ do
      let
        text = "ThisShouldBeLower1234"
      getKeyModifier @ToLower text `shouldBe` "thisshouldbelower1234"
    it "ToUpper" $ do
      let
        text = "ThisShouldBeUpper1234"
      getKeyModifier @ToUpper text `shouldBe` "THISSHOULDBEUPPER1234"
    it "LowerFirst" $ do
      let
        text = "ThisShouldBeLower1234"
      getKeyModifier @LowerFirst text `shouldBe` "thisShouldBeLower1234"
    it "UpperFirst" $ do
      let
        text = "thisShouldBeLower1234"
      getKeyModifier @UpperFirst text `shouldBe` "ThisShouldBeLower1234"
    it "StripPrefix" $ do
      let
        text = "someConvalutedRecordNameRecordValue"
      getKeyModifier @(StripPrefix "someConvalutedRecordName") text
        `shouldBe` "RecordValue"
    it "StripSuffix" $ do
      let
        text = "superCoolApplicationConfig"
      getKeyModifier @(StripSuffix "Config") text
        `shouldBe` "superCoolApplication"
    it "CamelToSnake" $ do
      let
        text = "superCoolApplicationConfig"
      getKeyModifier @CamelToSnake text
        `shouldBe` "super_cool_application_config"
    it "CamelToKebab" $ do
      let
        text = "superCoolApplicationConfig"
      getKeyModifier @CamelToKebab text
        `shouldBe` "super-cool-application-config"
    it "should work as a list, applied in left to right order" $ do
      let
        text = "superCoolApplicationConfig"
      getKeyModifier @[StripSuffix "Config", CamelToSnake, ToUpper] text
        `shouldBe` "SUPER_COOL_APPLICATION"
    it "should work as a tuple, applied in left to right order" $ do
      let
        text = "superCoolApplicationConfig"
      getKeyModifier @(StripSuffix "Config", CamelToSnake, ToUpper) text
        `shouldBe` "SUPER_COOL_APPLICATION"
