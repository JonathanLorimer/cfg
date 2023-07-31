module Cfg.EnvSpec where

import System.Environment (setEnv, unsetEnv)
import KeyTree
import Test.Hspec
import Data.Text (Text)
import Data.Map.Strict (singleton, fromList, empty)
import Cfg.Env

spec :: Spec
spec = around 
  (\runTest -> do
      setEnv "A_B" "Functor"
      setEnv "A_C" "Applicative"
      setEnv "A_D" "Monad"
      runTest ()
      unsetEnv "A_B"
      unsetEnv "A_C"
      unsetEnv "A_D"
  ) $ describe "envSource" $ do
      it "should get variables from environment" $ do
        let
          tree :: KeyTree Text Text = 
            Free $ singleton "A" $
              Free $ fromList 
                [ ("B", Free empty) 
                , ("C", Free empty) 
                , ("D", Free empty) 
                ]

        let
          expected =
            Free $ singleton "A" $
              Free $ fromList 
                [ ("B", Pure "Functor") 
                , ("C", Pure "Applicative") 
                , ("D", Pure "Monad") 
                ]
        result <- envSource tree
        result `shouldBe` expected
      it "should respect defaults" $ do
        unsetEnv "A_C"
        let
          tree :: KeyTree Text Text = 
            Free $ singleton "A" $
              Free $ fromList 
                [ ("B", Pure "Traversable") 
                , ("C", Pure "Applicative") 
                , ("D", Free empty) 
                ]

        let
          expected =
            Free $ singleton "A" $
              Free $ fromList 
                [ ("B", Pure "Functor") 
                , ("C", Pure "Applicative") 
                , ("D", Pure "Monad") 
                ]
        result <- envSource tree
        result `shouldBe` expected
