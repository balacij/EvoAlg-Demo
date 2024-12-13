module Lang.Math.Grammar.CST (grammar, parseExpr) where

import Lang.Math.Core
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

import Lang.Grammar.Core

import Control.Monad.Combinators.Expr

-- E -> E + T | E - T | T
-- T -> T * F | T / F | T % F | T ^ F | F
-- F -> ( E ) | x | -( E ) | abs( E ) | sin( E ) | cos( E ) | tan( E )

grammar :: Grammar
grammar =
  mkGrammar
    ["E", "T", "F"]
    ["x", "+", "-", "*", "/", "%", "abs", "sin", "cos", "tan", "(", ")"]
    "E"
    [ ( "E",
        [ [NonTerminal "E", Terminal "+", NonTerminal "T"],
          [NonTerminal "E", Terminal "-", NonTerminal "T"],
          [NonTerminal "T"]
        ]
      ),
      ( "T",
        [ [NonTerminal "T", Terminal "*", NonTerminal "F"],
          [NonTerminal "T", Terminal "/", NonTerminal "F"],
          [NonTerminal "T", Terminal "%", NonTerminal "F"],
          [NonTerminal "T", Terminal "^", NonTerminal "F"],
          [NonTerminal "F"]
        ]
      ),
      ( "F",
        [ [Terminal "(", NonTerminal "E", Terminal ")"],
          [Terminal "x"],
          [Terminal "-(", NonTerminal "E", Terminal ")"],
          [Terminal "abs(", NonTerminal "E", Terminal ")"],
          [Terminal "sin(", NonTerminal "E", Terminal ")"],
          [Terminal "cos(", NonTerminal "E", Terminal ")"],
          [Terminal "tan(", NonTerminal "E", Terminal ")"]
        ]
      )
    ]

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

pExpr :: Parser Expr
pExpr = makeExprParser pTerm tableE
  where
    tableE =
      [ [ InfixL (BinOp Add <$ symbol "+")
        , InfixL (BinOp Sub <$ symbol "-") ]
      ]

pTerm :: Parser Expr
pTerm = makeExprParser pFactor tableT
  where
    tableT =
      [ [ InfixR (BinOp Pow <$ symbol "^") ]
      , [ InfixL (BinOp Mul <$ symbol "*")
        , InfixL (BinOp Div <$ symbol "/")
        , InfixL (BinOp Rem <$ symbol "%") ]
      ]

pFactor :: Parser Expr
pFactor = choice
    [ parens pExpr
    , pUnary
    , pX
    ]

pUnary :: Parser Expr
pUnary = choice
    [ UnOp Neg <$ symbol "-" <*> parens pExpr
    , UnOp Abs <$ symbol "abs" <*> parens pExpr
    , UnOp Sin <$ symbol "sin" <*> parens pExpr
    , UnOp Cos <$ symbol "cos" <*> parens pExpr
    , UnOp Tan <$ symbol "tan" <*> parens pExpr
    ]

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof) ""
