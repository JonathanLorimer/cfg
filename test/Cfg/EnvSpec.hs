module Cfg.EnvSpec where

import Cfg.Env (envSource)
import Control.Monad.Identity
import Data.Text (Text)
import Data.Tree (Tree (..))
import System.Environment (setEnv)
import Test.Hspec
import Tree.Append

spec :: Spec
spec = do
  describe "appendLeaf" $ do
    it "should append the prefix of node keys as a leaf" $ do
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]
      let
        expected =
          Node
            "A"
            [ Node "B" [Node "AB" []]
            , Node "C" [Node "AC" []]
            , Node "D" [Node "AD" []]
            ]
      appendLeaf id (foldr (\x acc -> acc <> x) "") [] tree `shouldBe` expected

  describe "appendLeafA" $ do
    it "should append the prefix of node keys as a leaf with Identity" $ do
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]
      let
        expected =
          Node
            (Identity "A")
            [ Node (Identity "B") [Node (Identity "AB") []]
            , Node (Identity "C") [Node (Identity "AC") []]
            , Node (Identity "D") [Node (Identity "AD") []]
            ]
      appendLeafA (pure . foldr (\x acc -> acc <> x) "") [] tree `shouldBe` expected

  describe "travAppendLeafA" $ do
    it "should append the prefix of node keys as a leaf with Just" $ do
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]
      let
        expected =
          Just $
            Node
              "A"
              [ Node "B" [Node "AB" []]
              , Node "C" [Node "AC" []]
              , Node "D" [Node "AD" []]
              ]
      travAppendLeafA (pure . foldr (\x acc -> acc <> x) "") [] tree `shouldBe` expected

    it "should be Nothing when the leaves are Nothing" $ do
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]
      travAppendLeafA (const Nothing) [] tree `shouldBe` Nothing

  describe "envSource" $ do
    it "Should get environment variables that are in the environment" $ do
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]
      let
        expected =
          Just $
            Node
              "A"
              [ Node "B" [Node "AB" []]
              , Node "C" [Node "AC" []]
              , Node "D" [Node "AD" []]
              ]
      travAppendLeafA (pure . foldr (\x acc -> acc <> x) "") [] tree `shouldBe` expected

    it "should be Nothing when the leaves are Nothing" $ do
      setEnv "A_B" "Functor"
      setEnv "A_C" "Applicative"
      setEnv "A_D" "Monad"
      let
        tree :: Tree Text = Node "A" [Node "B" [], Node "C" [], Node "D" []]

      let
        expected =
          Node
            "A"
            [ Node "B" [Node "Functor" []]
            , Node "C" [Node "Applicative" []]
            , Node "D" [Node "Monad" []]
            ]
      result <- envSource tree
      result `shouldBe` expected
