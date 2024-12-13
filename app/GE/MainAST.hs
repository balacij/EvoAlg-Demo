module GE.MainAST (main) where

import qualified EvoAlg.Grammatical as GE
import qualified Lang.Math.Grammar.AST as M

import Data.Text (Text)
import Data.Maybe (fromMaybe)

import GE.Common

indEval :: GE.Genome -> Maybe (Double, Int)
indEval g = do
    (text, necessaryGenes) <- GE.genomeToText g M.grammar maxExprDepth
    expr <- either (const Nothing) Just (M.parseExpr text)
    let fitness = exprError expr
    if isInfinite fitness || isNaN fitness || fitness > 1e6
        then Nothing
        else Just (fitness + lengthPenalty g, necessaryGenes)

indPrnt :: GE.Genome -> Text
indPrnt g = maybe "" fst $ GE.genomeToText g M.grammar maxExprDepth

main :: IO ()
main = do
    trial <- getTrialText

    (_, gss) <- GE.evolve
        populationSize maxGenomeLength codonMax generations
        mutationRate crossoverRate pruneRate
        indEval
        indPrnt

    print $ last gss

    dumpStatistics ("run/ge-ast-" ++ targetFunctionName ++ "_" ++ trial ++ ".csv") gss
