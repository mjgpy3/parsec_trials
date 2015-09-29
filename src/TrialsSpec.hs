module TestTrials where

import Trials
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parsing lists" $ do
    it "works for empty" $ do
      parseText "()" `shouldParseTo` [TList []]
    it "ignores spaces" $ do
      parseText "  (  ) " `shouldParseTo` [TList []]
    it "parses a nested list" $ do
      parseText "(())" `shouldParseTo` [TList [TList []]]
  describe "parsing integers" $ do
    it  "works for 0" $ do
      parseText "0" `shouldParseTo` [TInt 0]
    it  "works for 1" $ do
      parseText "1" `shouldParseTo` [TInt 1]
    it  "works for 42" $ do
      parseText "42" `shouldParseTo` [TInt 42]
    it  "works for -42" $ do
      parseText "-42" `shouldParseTo` [TInt (-42)]
  describe "parsing floats" $ do
    it  "works for 42.2" $ do
      parseText "42.2" `shouldParseTo` [TFloat 42.2]
    it  "works for -42.2" $ do
      parseText "-42.2" `shouldParseTo` [TFloat (-42.2)]

shouldParseTo a b = a `shouldBe` Right b
