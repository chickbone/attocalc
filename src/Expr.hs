{-# LANGUAGE BangPatterns #-}

module Expr (Expr (..), optimize) where

data Expr a
  = Literal a
  | Unary (a -> a) (Expr a)
  | Binary (a -> a -> a) (Expr a) (Expr a)

{- evalExpr :: Expr a -> a
evalExpr (Literal a) = a
evalExpr (Unary f e) = f (evalExpr e)
evalExpr (Binary op l r) = evalExpr l `op` evalExpr r -}

optimize' :: Expr a -> Expr a
optimize' (Literal a) = Literal a
optimize' (Unary g (Unary f e)) = optimize' $ Unary (g . f) e -- compose
optimize' (Unary f (Binary op l r)) = optimize' $ Binary ((f .) . op) l r -- compose
optimize' (Binary op (Unary f l) r) = optimize' $ Binary (op . f) l r
optimize' (Binary op l (Unary f r)) = optimize' $ Binary (flip (flip op . f)) l r
optimize' (Binary op (Literal a) r) = optimize' $ Unary (a `op`) r -- left reduce
optimize' (Binary op l (Literal a)) = optimize' $ Unary (`op` a) l -- right reduce
optimize' (Unary f e) = Unary f (optimize' e)
optimize' (Binary op l r) = Binary op (optimize' l) (optimize' r)

optimize :: Eq a => Expr a -> Expr a
optimize expr = if expr' == expr then expr else optimize expr'
  where
    !expr' = optimize' expr

instance Show a => Show (Expr a) where
  show (Literal a) = "Literal " <> show a
  show (Unary _ e) = "func(" <> show e <> ")"
  show (Binary _ l r) = '(' : show l <> ") `op` (" <> show r <> ")"

instance Eq a => Eq (Expr a) where
  Literal a == Literal b = a == b
  Unary _ l == Unary _ r = l == r
  Binary _ l1 r1 == Binary _ l2 r2 = (l1 == l2) && (r1 == r2)
  _ == _ = False
