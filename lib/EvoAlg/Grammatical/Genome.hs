module EvoAlg.Grammatical.Genome (
    Genome(..),
    mkGenome, mkGenomeL,
    genomeToText, size
) where

import Lang.Grammar.Core (Grammar(start, productions), Symbol(..))

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map.Strict as M

type Codon = Int
newtype Genome = Genome { unGenome :: V.Vector Codon}
    deriving (Eq, Show)

mkGenome :: V.Vector Codon -> Genome
mkGenome = Genome

mkGenomeL :: [Codon] -> Genome
mkGenomeL cs = Genome (V.fromList cs)

size :: Genome -> Int
size (Genome cs) = V.length cs

type Depth = Int
type MaxDepth = Depth
type GenesExpressed = Int

genomeToText :: Genome -> Grammar -> MaxDepth -> Maybe (Text, GenesExpressed)
genomeToText (Genome cs) grmr maxDepth = 
    case derive (NonTerminal $ start grmr) maxDepth 0 0 of
        Just (result, finalIndex) -> Just (T.concat result, finalIndex)
        Nothing          -> Nothing
  where
    -- Recursive derivation function
    derive :: Symbol -> MaxDepth -> Depth -> Int -> Maybe ([Text], Int)
    derive (Terminal t) _ _ ci = Just ([t], ci)
    derive (NonTerminal nt) maxD depth codonIndex
        | depth > maxD = Nothing  -- Exceeded max depth
        | otherwise = case M.lookup nt (productions grmr) of
            Just rules ->
                if V.length rules == 1
                    then deriveSymbols (rules V.! 0) codonIndex (depth + 1)
                    else let codon = cs V.! (codonIndex `mod` V.length cs)
                             ruleIndex = codon `mod` V.length rules
                             selectedRule = rules V.! ruleIndex
                          in deriveSymbols selectedRule (codonIndex + 1) (depth + 1)
            Nothing -> Nothing  -- Nonterminal has no production rules

    deriveSymbols :: V.Vector Symbol -> Int -> Depth -> Maybe ([Text], Int)
    deriveSymbols symbols codonIndex depth =
        foldl' processSymbol (Just ([], codonIndex)) symbols
      where
        processSymbol :: Maybe ([Text], Int) -> Symbol -> Maybe ([Text], Int)
        processSymbol Nothing _ = Nothing
        processSymbol (Just (derivedTexts, currentIndex)) symbol =
            case derive symbol maxDepth depth currentIndex of
                Nothing -> Nothing
                Just (derivedNtTexts, nextIndex) ->
                    Just (derivedTexts ++ derivedNtTexts, nextIndex)
