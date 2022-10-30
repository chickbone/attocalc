{-# LANGUAGE OverloadedStrings #-}

module Main where

import Calc.AST (evalExpr)
import Control.Monad (unless)
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.ByteString as BS
import Data.Function (fix)
import Lib (calc, calcD)
import Options.Applicative (Parser, execParser, fullDesc, header, help, helper, info, long, progDesc, short, switch, (<**>))

main :: IO ()
main = mainloop =<< execParser opts
  where
    opts =
      info (flagDouble <**> helper) $
        fullDesc
          <> progDesc "Print a greeting for TARGET"
          <> header "This is a minimum calc."

mainloop :: Bool -> IO ()
mainloop isD =
  fix $ \loop -> do
    putStrLn "\x001B[1A"
    text <- BS.getLine
    putStrLn "\x001B[1A\x001B[0J"
    putStr $ "\x001B[0J= " <> (if isD then runCalcD text else runCalc text)
    putStr "\x001B[1A"
    unless (text == "q") loop

flagDouble :: Parser Bool
flagDouble =
  switch $
    long "double"
      <> short 'd'
      <> help "Use Double"

runCalc :: BS.ByteString -> String
runCalc bs = case parseOnly calc bs of
  Left e -> "error: " <> show e
  Right r -> show (evalExpr r)

runCalcD :: BS.ByteString -> String
runCalcD bs = case parseOnly calcD bs of
  Left e -> "error: " <> show e
  Right r -> show r
