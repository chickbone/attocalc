{-# LANGUAGE DeriveFunctor #-}

module Calc.AST where

data CExpr a
  = Lit a
  | Add (CExpr a) (CExpr a)
  | Sub (CExpr a) (CExpr a)
  | Mul (CExpr a) (CExpr a)
  | Div (CExpr a) (CExpr a)
  | Pow (CExpr a) (CExpr a)
  deriving (Eq, Functor)

evalExpr :: CExpr Int -> Int
evalExpr (Lit a) = a
evalExpr (Add l r) = evalExpr l + evalExpr r
evalExpr (Sub l r) = evalExpr l - evalExpr r
evalExpr (Mul l r) = evalExpr l * evalExpr r
evalExpr (Div l r) = evalExpr l `div` evalExpr r
evalExpr (Pow l r) = evalExpr l ^ evalExpr r

instance Show a => Show (CExpr a) where
  show (Lit a) = show a
  show (Add l r) = show l <> " + " <> show r
  show (Sub l r) = show l <> " - " <> show r
  show (Mul l r) = show l <> " * " <> show r
  show (Div l r) = show l <> " / " <> show r
  show (Pow l r) = show l <> " ^ " <> show r
