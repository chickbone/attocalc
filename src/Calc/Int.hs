
module Calc.Int (calc) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many', many1)
import Data.Attoparsec.ByteString.Char8 (char, digit)
import Data.Function (fix)
import Calc.Common ( eval, apply, addition, factor )

num :: Parser Integer
num = read <$> many1 digit

term :: Integral a => Parser a -- ^ continue parser
  -> Parser a
term f =
  eval f . many' $
    char '*' *> apply (*) f
      <|> char '/' *> apply div f

pow :: Integral a => Parser a -- ^ continue parser
  -> Parser a
pow f =
  eval f . many' $
    char '^' *> apply (^) f

calc :: Parser Integer
calc = fix (\f -> addition . term . pow $ factor f num)
