module Lang.Math.Eval (evalExpr) where

import Data.Fixed (mod')
import Lang.Math.Core

evalExpr :: Expr -> Double -> Double
evalExpr X x = x
evalExpr (UnOp uo e) x = unOp uo $ evalExpr e x
evalExpr (BinOp bo le re) x = binOp bo (evalExpr le x) (evalExpr re x)

unOp :: UnOp -> (Double -> Double)
unOp Neg = \x -> -x
unOp Abs = abs
unOp Sin = sin
unOp Cos = cos
unOp Tan = tan

binOp :: BinOp -> (Double -> Double -> Double)
binOp Add = (+)
binOp Sub = (-)
binOp Mul = (*)
binOp Div = (/)
binOp Rem = \l r -> if abs r <= 1e-2 then 1/0 else l `mod'` r
binOp Pow = (**)
