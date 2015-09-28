module TestTrials where

import Trials
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parsing lists" $ do
    it "works for empty" $ do
      parseText "()" `shouldParseTo` TList []

shouldParseTo a b = a `shouldBe` Right b
