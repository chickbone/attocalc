{-# LANGUAGE OverloadedStrings #-}

module Calc.Double (calcD) where

import Calc.Common (addition, apply, eval, factor)
import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many')
import Data.Attoparsec.ByteString.Char8 (char, double)
import Data.Function (fix)

term, pow :: Parser Double -> Parser Double
term f =
  eval f . many' $
    char '*' *> apply (*) f
      <|> char '/' *> apply (/) f
pow f =
  eval f . many' $
    char '^' *> apply (**) f

calcD :: Parser Double
calcD = fix $ addition . term . pow . factor double
