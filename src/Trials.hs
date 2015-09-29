module Trials where

import Text.ParserCombinators.Parsec

data TExpr = TList [TExpr]
  | TInt Int deriving (Show, Eq)

trials :: Parser TExpr
trials = number
  <|> list

number :: Parser TExpr
number = negativeInt <|> int

negativeInt :: Parser TExpr
negativeInt = do
  char '-'
  t <- many1 digit
  return $ TInt $ (read ('-':t) :: Int)

int :: Parser TExpr
int = do
  t <- many1 digit
  return $ TInt $ (read t :: Int)

list :: Parser TExpr
list = do
  char '('
  char ')'
  return $ TList []

parseText = parse trials ""
