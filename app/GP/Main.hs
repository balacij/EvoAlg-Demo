module GP.Main (main) where

import qualified EvoAlg.TreeGenetic as TGP

import GE.Common hiding (dumpStatistics, maxExprDepth)

import qualified Lang.Math as M

import qualified Data.Vector as V
import Data.Csv (ToNamedRecord(..), (.=), namedRecord, encodeByName, NamedRecord)

import qualified Data.ByteString.Lazy as BL

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

depthPenaltyMultiplier :: Double
depthPenaltyMultiplier = 0.01 -- id -- TODO: stronger penalty?

indEval :: M.Expr -> Maybe (Double, Int)
indEval e = if isInfinite fitness || isNaN fitness || fitness > 1e6
                then Nothing
                else Just (fitness + depthPenaltyMultiplier * fromIntegral depth, depth)
    where
        fitness = exprError e
        depth = M.depth e

maxExprDepth :: Int
maxExprDepth = 10

main :: IO ()
main = do
    trial <- getTrialText

    (_, gss) <- TGP.evolve
        populationSize maxExprDepth generations
        mutationRate crossoverRate pruneRate
        indEval
        M.toText

    print $ last gss

    dumpStatistics ("run/gp-" ++ targetFunctionName ++ "_" ++ trial ++ ".csv") gss


dumpStatistics :: String -> [TGP.GenerationStatistics] -> IO ()
dumpStatistics trgtFile gss = do
    let parentDir = takeDirectory trgtFile
    createDirectoryIfMissing True parentDir

    putStrLn "Writing to: "
    print trgtFile

    let hdr = V.fromList [ "generation"
                         , "populationSize"
                         , "invalidInds"
                         , "localBest"
                        --  , "localBestInd"
                         , "localBestFit"
                         , "fitnessAvg"
                         , "fitnessStdDev"
                         , "exprDepthAvg"
                         , "top10PercExprDepthAvg"
                         , "top10PercFitnessAvg"
                         , "top10PercFitnessStdDev" ]

    BL.writeFile trgtFile $ encodeByName hdr gss

instance ToNamedRecord TGP.GenerationStatistics where
     toNamedRecord :: TGP.GenerationStatistics -> NamedRecord
     toNamedRecord g = namedRecord [ "generation" .= TGP.generation g
                                   , "populationSize" .= TGP.populationSize g
                                   , "invalidInds" .= TGP.invalidInds g
                                   , "localBest" .= TGP.localBest g
                                --    , "localBestInd" .= (show . TGP.unGenome) (TGP.repr $ TGP.localBestInd g) -- TODO: Ugh
                                   , "localBestFit" .= TGP.localBestFit g -- TODO: Ugh
                                   , "fitnessAvg" .= TGP.fitnessAvg g
                                   , "fitnessStdDev" .= TGP.fitnessStdDev g
                                   , "exprDepthAvg" .= TGP.exprDepthAvg g
                                   , "top10PercExprDepthAvg" .= TGP.top10PercExprDepthAvg g
                                   , "top10PercFitnessAvg" .= TGP.top10PercFitnessAvg g
                                   , "top10PercFitnessStdDev" .= TGP.top10PercFitnessStdDev g ]
