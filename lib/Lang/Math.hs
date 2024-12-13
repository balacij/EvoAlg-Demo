module Lang.Math (
    -- * Core
    Expr(..), BinOp(..), UnOp(..), 
    depth, toText,
    
    -- * Eval
    evalExpr
) where

import Lang.Math.Core (Expr(..), BinOp(..), UnOp(..), depth, toText)
import Lang.Math.Eval (evalExpr)
