module Lang.Grammar.Core (Grammar, terminals, nonterminals, start, productions, mkGrammar, Symbol(..)) where

import Data.Text (Text)
import Data.Vector
import Data.Map.Strict
import Data.Set

import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Symbol = NonTerminal Text
            | Terminal Text
            deriving (Show, Eq)

data Grammar = Grammar { terminals    :: Set Text
                       , nonterminals :: Set Text
                       , start        :: Text
                       , productions  :: Map Text (Vector (Vector Symbol))
                       }

-- TODO: I can add a bunch of sanity checks here
mkGrammar :: [Text] -> [Text] -> Text -> [(Text, [[Symbol]])] -> Grammar
mkGrammar ts nts st ps =
        Grammar (S.fromList ts)
                (S.fromList nts)
                st
                (M.fromList ps')
    where
        ps' :: [(Text, Vector (Vector Symbol))]
        ps' = (\(t, sss) -> (t, V.fromList $ V.fromList <$> sss)) <$> ps

