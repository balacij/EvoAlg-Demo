module Lang.Grammar.Examples (asAndBs) where

import Lang.Grammar.Core

asAndBs :: Grammar
asAndBs = mkGrammar
            ["a", "b"]
            ["S", "A", "B"]
            "S"
            [ ("S", [[NonTerminal "A", NonTerminal "B"]])
            , ("A", [ [ Terminal "a", NonTerminal "A"]
                    , [Terminal "a"]])
            , ("B", [ [Terminal "b", NonTerminal "B"]
                    , [Terminal "b"]])]
