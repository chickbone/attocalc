module Calc.Int (calc) where

import Calc.Common (addition, apply, eval, factor)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many', many1)
import Data.Attoparsec.ByteString.Char8 (char, digit)
import Data.Function (fix)

num :: Parser Integer
num = read <$> many1 digit

term ::
  Integral a =>
  -- | continue parser
  Parser a ->
  Parser a
term f =
  eval f . many' $
    char '*' *> apply (*) f
      <|> char '/' *> apply div f

pow ::
  Integral a =>
  -- | continue parser
  Parser a ->
  Parser a
pow f =
  eval f . many' $
    char '^' *> apply (^) f

calc :: Parser Integer
calc = fix (\f -> addition . term . pow $ factor f num)
