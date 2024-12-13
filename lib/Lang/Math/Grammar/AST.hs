module Lang.Math.Grammar.AST (grammar, parseExpr) where

import Lang.Grammar (Grammar, mkGrammar, Symbol (..))

import Lang.Math.Core

import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void

grammar :: Grammar
grammar =
    mkGrammar
        ["E", "UnOp", "BinOp"]
        ["x", "+", "-", "*", "/", "%", "abs", "sin", "cos", "tan", "(", ")"]
        "E"
        [("E", [ [Terminal "x"]
               , [NonTerminal "UnOp", Terminal "(", NonTerminal "E", Terminal ")"]
               , [Terminal "(", NonTerminal "E", Terminal ")", NonTerminal "BinOp", Terminal "(", NonTerminal "E", Terminal ")"]
               ])
        ,("UnOp", [ [Terminal "-"]
                  , [Terminal "abs"]
                  , [Terminal "sin"]
                  , [Terminal "cos"]
                  , [Terminal "tan"]
                  ])
        ,("BinOp", [ [Terminal "+"]
                   , [Terminal "-"]
                   , [Terminal "*"]
                   , [Terminal "/"]
                   , [Terminal "%"]
                   , [Terminal "^"]
                   ])]

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pX :: Parser Expr
pX = X <$ lexeme (char 'x')

pUnOp :: Parser UnOp
pUnOp = lexeme (choice
    [ Neg <$ char '-'
    , Abs <$ string "abs"
    , Sin <$ string "sin"
    , Cos <$ string "cos"
    , Tan <$ string "tan"
    ])

pPrimary :: Parser Expr
pPrimary = choice
    [ pX
    , parens pExpr
    ]

pUnary :: Parser Expr
pUnary = do
    op <- pUnOp
    expr <- parens pExpr
    return $ UnOp op expr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm table
  where
    pTerm = choice [pUnary, pPrimary]

    table =
      [ [ InfixR (BinOp Pow <$ symbol "^") ] -- Right-associative for Pow
      , [ InfixL (BinOp Mul <$ symbol "*")
        , InfixL (BinOp Div <$ symbol "/")
        , InfixL (BinOp Rem <$ symbol "%") ]
      , [ InfixL (BinOp Add <$ symbol "+")
        , InfixL (BinOp Sub <$ symbol "-") ]
      ]

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof) ""
