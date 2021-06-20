{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Monoid ((<>))
import Lib
import Text.Read (readMaybe)

type Wizard a = IO (IO a)

main :: IO ()
main =
  fix $ \loop -> do
    putStrLn "\x001B[1A"
    text <- getLine
    putStrLn "\x001B[1A\x001B[0J"
    putStr . ("\x001B[0J= " <>) . show . eval $ text
    putStr "\x001B[1A"
    unless (text == "q") loop

endless :: Monad m => m a -> m b
endless f = fix $ \loop -> f >> loop

push :: a -> State [a] ()
push x = modify (x :)

pop :: State [a] a
pop = get >>= (\xs -> put (tail xs) >> return (head xs))

op f = do
  x <- pop
  y <- pop
  push $ f x y

rpn ("+" : xs) = op (+) >> rpn xs
rpn ("*" : xs) = op (*) >> rpn xs
rpn ("-" : xs) = op (-) >> rpn xs
rpn (x : xs) = push (read x) >> rpn xs
rpn [] = pop

check :: [String] -> [String]
check str = if null text then ["0"] else text
  where
    text = filter (\x -> isJust (readMaybe x :: Maybe Integer) || x == "+" || x == "*" || x == "-") str

eval :: String -> Int
eval = (`evalState` []) . rpn . check . words

quest :: String -> Wizard ()
quest str = do
  putStrLn $ str <> "?: "
  x <- getLine
  return $ putStrLn $ str <> ": " <> x

runWizard :: Wizard a -> IO a
runWizard = join
