module TestTrials where

import Trials
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parsing lists" $ do
    it "works for empty" $ do
      parseText "()" `shouldParseTo` TList []
  describe "parsing integers" $ do
    it  "works for 0" $ do
      parseText "0" `shouldParseTo` TInt 0
    it  "works for 1" $ do
      parseText "1" `shouldParseTo` TInt 1

shouldParseTo a b = a `shouldBe` Right b
