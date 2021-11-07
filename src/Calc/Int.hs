module Calc.Int (calc) where

import Calc.AST
import Calc.Common (apply, eval, factor)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many')
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.Function (fix)

term :: Parser (CExpr Int) -> Parser (CExpr Int)
term f =
  eval f . many' $
    char '*' *> apply Mul f
      <|> char '/' *> apply Div f

pow :: Parser (CExpr Int) -> Parser (CExpr Int)
pow f =
  eval f . many' $
    char '^' *> apply Pow f

additionC :: Parser (CExpr a) -> Parser (CExpr a)
additionC f =
  eval f . many' $
    char '+' *> apply Add f
      <|> char '-' *> apply Sub f

calc :: Parser (CExpr Int)
calc = fix $ additionC . term . pow . factor (Lit <$> decimal)
