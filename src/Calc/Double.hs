module Calc.Double (calcD) where

import Calc.Common (addition, apply, eval, factor)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many', many1)
import Data.Attoparsec.ByteString.Char8 (char, digit)
import Data.Function (fix)

numD :: Parser Double
numD = read <$> many1 digit

termD ::
  Fractional a =>
  -- | continue parser
  Parser a ->
  Parser a
termD f =
  eval f . many' $
    char '*' *> apply (*) f
      <|> char '/' *> apply (/) f

powD ::
  Floating a =>
  -- | continue parser
  Parser a ->
  Parser a
powD f =
  eval f . many' $
    char '^' *> apply (**) f

calcD :: Parser Double
calcD = fix (\f -> addition . termD . powD $ factor f numD)
