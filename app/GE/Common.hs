module GE.Common where

import qualified EvoAlg.Grammatical as GE

import Lang.Math (Expr, evalExpr)

import qualified Data.Vector as V
import Data.Csv (ToNamedRecord(..), (.=), namedRecord, encodeByName, NamedRecord)

import qualified Data.ByteString.Lazy as BL

import Data.Maybe (fromMaybe)

import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.Environment (lookupEnv)

getTrialText :: IO String
getTrialText = do
    mbTrial <- lookupEnv "TRIAL"
    return $ take 10 $ fromMaybe "x" mbTrial

populationSize, maxGenomeLength, codonMax, generations :: Int
populationSize = 200
maxGenomeLength = 120
codonMax = 255
generations = 3500

mutationRate, crossoverRate, pruneRate :: Double
mutationRate = 0.05
crossoverRate = 0.9
pruneRate = 0.05

maxExprDepth :: Int
maxExprDepth = 30

targetFunctionName :: String
targetFunction :: Double -> Double
sampleAt :: [Double]
lengthPenaltyMultiplier :: Double

-- 2x+3
-- targetFunctionName = "2x+3"
-- targetFunction x = 2 * x + 3
-- sampleAt = map (* (1/3)) [-10..10] -- 21 sample points
-- lengthPenaltyMultiplier = 1

-- x*(x+tanx)
-- targetFunctionName = "x*(x+tanx)"
-- targetFunction x = x * x + x * tan x
-- sampleAt = map (* (1/3)) [-15..15] -- 31 sample points
-- lengthPenaltyMultiplier = 1

-- cosx+sin2x
-- targetFunctionName = "cosx+sin2x"
-- targetFunction x = cos x + sin (2 * x)
-- sampleAt = map (* (1/10)) [-20..20] -- 41 sample points
-- lengthPenaltyMultiplier = 0.01

-- xsin(x^2)
-- targetFunctionName = "xsin(x^2)"
-- targetFunction x = x * sin (x ^ 2)
-- sampleAt = map (* (1/3)) [-15..15] -- 31 sample points
-- lengthPenaltyMultiplier = 1

-- x/(1+x^2)
targetFunctionName = "xOVER(1+x^2)"
targetFunction x = x / (1 + (x ** 2))
sampleAt = map (* (1/25)) [-150..150] -- 301 sample points
lengthPenaltyMultiplier = 0.01

-- sin(x) + floor(x)
-- targetFunctionName = "sinx+floorx"
-- targetFunction x = sin x + fromIntegral (truncate x :: Int)
-- sampleAt = map (* (1/3)) [-25..25] -- 51 sample points
-- lengthPenaltyMultiplier = 0.01

samples :: [(Double, Double)]
samples = zip sampleAt $ map targetFunction sampleAt

exprError :: Expr -> Double
exprError e = sum $ map (\(x, truth) -> abs $ truth - evalExpr e x) samples

lengthPenalty :: GE.Genome -> Double
lengthPenalty (GE.Genome g) = lengthPenaltyMultiplier * fromIntegral (V.length g)

dumpStatistics :: String -> [GE.GenerationStatistics] -> IO ()
dumpStatistics trgtFile gss = do
    let parentDir = takeDirectory trgtFile
    createDirectoryIfMissing True parentDir

    putStrLn "Writing to: "
    print trgtFile

    let hdr = V.fromList [ "generation"
                         , "populationSize"
                         , "invalidInds"
                         , "localBest"
                         , "localBestInd"
                         , "localBestFit"
                         , "fitnessAvg"
                         , "fitnessStdDev"
                         , "genomeSizeAvg"
                         , "effectiveGenomeSizeAvg"
                         , "top10PercEffectiveGenomeSizeAvg"
                         , "top10PercFitnessAvg"
                         , "top10PercFitnessStdDev" ]

    BL.writeFile trgtFile $ encodeByName hdr gss

instance ToNamedRecord GE.GenerationStatistics where
     toNamedRecord :: GE.GenerationStatistics -> NamedRecord
     toNamedRecord g = namedRecord [ "generation" .= GE.generation g
                                   , "populationSize" .= GE.populationSize g
                                   , "invalidInds" .= GE.invalidInds g
                                   , "localBest" .= GE.localBest g
                                   , "localBestInd" .= (show . GE.unGenome) (GE.repr $ GE.localBestInd g) -- TODO: Ugh
                                   , "localBestFit" .= GE.localBestFit g -- TODO: Ugh
                                   , "fitnessAvg" .= GE.fitnessAvg g
                                   , "fitnessStdDev" .= GE.fitnessStdDev g
                                   , "genomeSizeAvg" .= GE.genomeSizeAvg g
                                   , "effectiveGenomeSizeAvg" .= GE.effectiveGenomeSizeAvg g
                                   , "top10PercEffectiveGenomeSizeAvg" .= GE.top10PercEffectiveGenomeSizeAvg g
                                   , "top10PercFitnessAvg" .= GE.top10PercFitnessAvg g
                                   , "top10PercFitnessStdDev" .= GE.top10PercFitnessStdDev g ]

