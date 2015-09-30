module TestTrials where

import Data.Foldable (for_)
import Trials
import Test.Hspec

main :: IO ()
main = hspec $ do
  for_ [("(", ")"), ("[", "]")] $ \(o, c) ->
    describe ("parsing lists using " ++ o ++ c) $ do
      it "works for empty" $ do
        parseText (o ++ c) `shouldParseTo` [TList []]
      it "ignores spaces" $ do
        parseText ("  " ++ o ++ "  " ++ c ++ " ") `shouldParseTo` [TList []]
      it "parses a nested list" $ do
        parseText (o ++ o ++ c ++ c) `shouldParseTo` [TList [TList []]]
      it "parses _crazy_ nested lists" $ do
        parseText (o ++ o ++ o ++ c ++ o ++ c ++ o ++ c ++ c ++ c) `shouldParseTo` [TList [TList [TList [], TList [], TList []]]]

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

  describe "parsing strings" $ do
    it  "works for the empty string" $ do
      parseText "\"\"" `shouldParseTo` [TString []]

  describe "parsing symbols" $ do
    it "works for :" $ do
      parseText ":" `shouldParseTo` [TSymbol ":"]
    it "works for multi character symbols" $ do
      parseText "abcedfg" `shouldParseTo` [TSymbol "abcedfg"]
    it "works for separated symbols" $ do
      parseText "cow dog" `shouldParseTo` [TSymbol "cow", TSymbol "dog"]
    it "works for -" $ do
      parseText "-" `shouldParseTo` [TSymbol "-"]
    it "can split itself from numbers" $ do
      parseText "-4abc" `shouldParseTo` [TInt (-4), TSymbol "abc"]
    it "can parse symbols containing digits" $ do
      parseText "asym42" `shouldParseTo` [TSymbol "asym42"]
    it "can parse strange, purely symbolic symbols" $ do
      parseText "<-~!!@@~->" `shouldParseTo` [TSymbol "<-~!!@@~->"]

  describe "parsing combinations" $ do
    it "works for applications of subtraction" $ do
      parseText "(- 45 3)" `shouldParseTo` [TList [TSymbol "-", TInt 45, TInt 3]]

  describe "parsing failures" $ do
    it "when mixing '()'s and '[]'s inappropriately" $ do
      shouldError $ parseText "(]"

shouldParseTo a b = a `shouldBe` Right b
shouldError (Right _) = 1 `shouldBe` 2
shouldError (Left _) = 1 `shouldBe` 1
