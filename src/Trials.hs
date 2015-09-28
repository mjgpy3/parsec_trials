module Trials where

import Text.ParserCombinators.Parsec

data TExpr = TList [TExpr]
  | TInt Int deriving (Show, Eq)

trialsParser :: Parser TExpr
trialsParser = intParser
  <|> listParser

intParser :: Parser TExpr
intParser = do
  char '0'
  return $ TInt 0

listParser :: Parser TExpr
listParser = do
  char '('
  char ')'
  return $ TList []

parseText = parse trialsParser ""
