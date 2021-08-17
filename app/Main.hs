{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Function (fix)
import Data.Attoparsec.ByteString (parseOnly)
import Lib

main :: IO ()
main = putStrLn "This is a minimum calc. Please enter some expr:" *> mainloop

mainloop :: IO ()
mainloop =
  fix $ \loop -> do
    putStrLn "\x001B[1A"
    text <- BS.getLine
    putStrLn "\x001B[1A\x001B[0J"
    putStr $ "\x001B[0J= " <> runCalc text
    putStr "\x001B[1A"
    unless (text == "q") loop

runCalc :: BS.ByteString -> String
runCalc bs = case parseOnly calc bs of
  Left e -> "error: " <> show e
  Right r -> show r
