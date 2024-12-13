module Lang.Math.Core (UnOp(..), BinOp(..), Expr(..), depth, toText) where

import qualified Data.Text as T

data UnOp = Neg | Abs | Sin | Cos | Tan
    deriving (Eq, Show)

data BinOp = Add | Sub | Mul | Div | Rem | Pow
    deriving (Eq, Show)

data Expr = X
          | UnOp UnOp Expr
          | BinOp BinOp Expr Expr
    deriving (Eq, Show)


depth :: Expr -> Int
depth X = 1
depth (UnOp _ e) = 1 + depth e
depth (BinOp _ l r) = 1 + max (depth l) (depth r)

toText :: Expr -> T.Text
toText e = T.concat $ toText' e

toText' :: Expr -> [T.Text]
toText' X = ["x"]
toText' (UnOp op sub) = [unOpToText op, "("] ++ toText' sub ++ [")"]
toText' (BinOp op l r) = ["("] ++ toText' l ++ [")", binOpToText op, "("] ++ toText' r ++ [")"]

unOpToText :: UnOp -> T.Text
unOpToText Neg = "-"
unOpToText Abs = "abs"
unOpToText Sin = "sin"
unOpToText Cos = "cos"
unOpToText Tan = "tan"

binOpToText :: BinOp -> T.Text
binOpToText Add = "+"
binOpToText Sub = "-"
binOpToText Mul = "*"
binOpToText Div = "/"
binOpToText Rem = "%"
binOpToText Pow = "^"
