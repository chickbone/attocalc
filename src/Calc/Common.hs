module Calc.Common (eval,apply,addition,factor) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many')
import Data.Attoparsec.ByteString.Char8 (char, skipSpace)

eval :: (Applicative f, Foldable t) => f b -> f (t (b -> b)) -> f b
eval m ops = foldl (\x f -> f x) <$> m <*> ops

apply :: Functor f => (a1 -> a2 -> c) -> f a2 -> f (a1 -> c)
apply op m = flip op <$> m

addition :: Num a => Parser a -- ^ continue parser
  -> Parser a
addition f =
  eval f . many' $
    char '+' *> apply (+) f
      <|> char '-' *> apply (-) f

factor :: Parser a -> Parser a -> Parser a
factor f n = skipSpace *> (char '(' *> f <* char ')' <|> n) <* skipSpace

