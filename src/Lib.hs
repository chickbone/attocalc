module Lib (runCalc) where

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString (Parser, many', many1, parseOnly)
import Data.Attoparsec.ByteString.Char8 (char, digit, skipSpace)
import Data.ByteString (ByteString)

num :: Parser Integer
num = read <$> many1 digit

eval :: (Applicative f, Foldable t) => f b -> f (t (b -> b)) -> f b
eval m ops = foldl (\x f -> f x) <$> m <*> ops

apply :: Functor f => (a1 -> a2 -> c) -> f a2 -> f (a1 -> c)
apply op m = flip op <$> m

addition :: Parser Integer
addition =
  eval term . many' $
    char '+' *> apply (+) term
      <|> char '-' *> apply (-) term

term :: Parser Integer
term =
  eval pow . many' $
    char '*' *> apply (*) pow
      <|> char '/' *> apply div pow

pow :: Parser Integer
pow =
  eval factor . many' $
    char '^' *> apply (^) factor

factor :: Parser Integer
factor = skipSpace *> (char '(' *> addition <* char ')' <|> num) <* skipSpace

runCalc :: ByteString -> String
runCalc bs = case parseOnly addition bs of
  Left e -> "error: " <> show e
  Right r -> show r
