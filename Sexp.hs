-- module Sexp (
--   showSexp
--   ) where

import Data.List (intersperse)

data Op = Add | Sub | Mul | Div
        deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Exp a = Number a
           | BinOp Op (Exp a) (Exp a)
           | ListOp Op [(Exp a)]
           | UnaryOp Op (Exp a)
             deriving (Eq, Show)

instance Num a => Num (Exp a) where
  (+) = BinOp Add
  (*) = BinOp Mul
  (-) = BinOp Sub
  negate a = UnaryOp Sub a
  abs = undefined
  signum = undefined
  fromInteger = Number . fromIntegral

instance Fractional a => Fractional (Exp a) where
  (/) = BinOp Div
  recip = (1 /)
  fromRational = Number . fromRational

showSexp (Number n) = show n
showSexp (BinOp op a b) = "(" ++ show op ++ " " ++ showArgs [a, b] ++ ")"
showSexp (UnaryOp op arg) = "(" ++ show op ++ " " ++ showSexp arg ++ ")"
showSexp (ListOp op args) = "(" ++ show op ++ " " ++ showArgs args ++ ")"

showArgs = concat . intersperse " " . map showSexp

showSexp' = showSexp . simplify

simplify (BinOp op a0 b0) =
  let a' = simplify a0
      b' = simplify b0
  in case (op, a', b') of
    (Mul, Number 1, b) -> b
    (Mul, a, Number 1) -> a
    (Mul, Number 0, _) -> Number 0
    (Mul, _, Number 0) -> Number 0
    (Add, Number 0, b) -> b
    (Add, a, Number 0) -> a
    (Sub, a, Number 0) -> a
    (Div, a, Number 1) -> a
    (_, (ListOp op' as), (ListOp op'' bs)) | op == op' && op == op'' -> ListOp op (as ++ bs)
    (_, (ListOp op' as), b) | op == op' -> ListOp op (as ++ [b]) 
    (_, a, (ListOp op' bs)) | op == op' -> ListOp op (a:bs)
    _ -> ListOp op [a', b']
simplify (UnaryOp op a) = UnaryOp op (simplify a)
simplify (ListOp op xs) = ListOp op (map simplify xs)
simplify x = x
