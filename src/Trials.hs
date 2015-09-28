module Trials where

import Text.ParserCombinators.Parsec

data TList = TList [TList] deriving (Show, Eq)

trialsParser :: Parser TList
trialsParser = do
  char '('
  char ')'
  return $ TList []

parseText = parse trialsParser ""
