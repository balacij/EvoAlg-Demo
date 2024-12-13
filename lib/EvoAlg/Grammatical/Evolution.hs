{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module EvoAlg.Grammatical.Evolution (
    generateRandomGenome, generateGenomePopulation,
    crossover, mutate, evolve,
    Individual, repr, necessaryGenes, fitness,
    GenerationStatistics(..)
) where

import EvoAlg.Grammatical.Genome (Genome(..))

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA

import System.Random (randomRIO)

import Data.Maybe (isJust, fromMaybe)
import Text.Printf (printf)
import Data.Text (Text)
import qualified EvoAlg.Grammatical.Genome as G

newtype Individual = Individual (Genome, Maybe (Double, Int))
    deriving (Eq, Show)

repr :: Individual -> Genome
repr (Individual (r, _)) = r

necessaryGenes :: Individual -> Maybe Int
necessaryGenes (Individual (_, Just (_, n))) = Just n
necessaryGenes _                             = Nothing

fitness :: Individual -> Maybe Double
fitness (Individual (_, Just (f, _))) = Just f
fitness _                             = Nothing

data GenerationStatistics = GS {
    generation :: Int,

    populationSize :: Int,
    invalidInds :: Int,

    localBest :: Text,
    localBestInd :: Individual,
    localBestFit :: Double,

    fitnessAvg :: Double,
    fitnessStdDev :: Double,

    genomeSizeAvg :: Double,
    effectiveGenomeSizeAvg :: Double,

    top10PercEffectiveGenomeSizeAvg :: Double,
    top10PercFitnessAvg :: Double,
    top10PercFitnessStdDev :: Double
}
    deriving (Eq, Show)

filterBad :: V.Vector Individual -> V.Vector Individual
filterBad = V.filter (\(Individual (_, f)) -> isJust f)

dashes :: IO ()
dashes = putStrLn $ replicate 120 '-'

genomeToIndividual :: (Genome -> Maybe (Double, Int)) -> Genome -> Individual
genomeToIndividual f g = Individual (g, f g)

freshPopulation :: Int -> Int -> Int -> (Genome -> Maybe (Double, Int)) -> IO (V.Vector Individual)
freshPopulation popGen genomeLen codonMax indEval = V.map (genomeToIndividual indEval) <$> generateGenomePopulation popGen genomeLen codonMax

evolve :: Int -> Int -> Int -> Int -> Double -> Double -> Double -> (Genome -> Maybe (Double, Int)) -> (Genome -> Text) -> IO (V.Vector Individual, [GenerationStatistics])
evolve _ _ _ 0 _ _ _ _ _ = pure (V.empty, []) -- No generations allowed
evolve popGen genomeLen codonMax maxGens mutRate xRate pruneRate indEval indPrtr = do
    -- Generate the initial population
    initialPop <- sortPopulation <$> freshPopulation popGen genomeLen codonMax indEval
    let initialStats = calculatePopStatistics 0 indPrtr initialPop

    -- Print configuration
    dashes
    putStrLn "Grammatical Evolution"
    dashes
    putStrLn "Configuration:"
    printf "    Population Size: %d\n" popGen
    printf "    Max Generations: %d\n" maxGens
    printf "    Max Genome Length: %d\n" genomeLen
    printf "    Max Codon Size: %d\n" codonMax
    printf "    Mutation Rate: %.f%%\n" (mutRate * 100)
    printf "    Crossover Rate: %.f%%\n" (xRate * 100)
    printf "    Prune Rate: %.f%%\n" (pruneRate * 100)
    dashes
    putStrLn "Initial Population:"
    print initialPop
    dashes
    everything <- evolve' initialPop [initialStats] 1
    dashes
    return everything
    where
        evolve' :: V.Vector Individual -> [GenerationStatistics] -> Int -> IO (V.Vector Individual, [GenerationStatistics])
        evolve' pop stats gen
            | gen > maxGens = return (pop, reverse stats)
            | otherwise = do
                -- Generate the next generation
                nextPop <- nextGeneration pop
                let nextStats = calculatePopStatistics gen indPrtr nextPop

                -- Print generation info
                printf "| Gen %d | Avg Fitness: %.2f | StdDev: %.2f | Pop Size: %d | Best Fitness: %.2f | %s\n"
                    gen
                    (fitnessAvg nextStats)
                    (fitnessStdDev nextStats)
                    (V.length nextPop)
                    (fromMaybe 0.0 (fitness (localBestInd nextStats)))
                    (localBest nextStats)

                -- Continue to the next generation
                evolve' nextPop (nextStats : stats) (gen + 1)

        nextGeneration :: V.Vector Individual -> IO (V.Vector Individual)
        nextGeneration start = do
            elites <- V.mapM pruneInd $ V.take (popGen `div` 6) start
            freshPop <- freshPopulation (popGen `div` 6) genomeLen codonMax indEval

            offspring <- V.replicateM ((popGen - V.length elites - V.length freshPop) `div` 2) $ do
                p1 <- selectParent start -- pulling from whole population
                p2 <- selectParent start -- pulling from whole population
                (c1, c2) <- crossoverInds p1 p2 indEval
                c1' <- mutateInd c1 indEval >>= pruneInd
                c2' <- mutateInd c2 indEval >>= pruneInd
                return (c1', c2')

            let flattenedOffspring = V.fromList (concatMap (\(c1, c2) -> [c1, c2]) (V.toList offspring))

            return $ sortPopulation $ V.concat [elites, flattenedOffspring, freshPop]

        selectParent :: V.Vector Individual -> IO Individual
        selectParent pop = do
            tournamentSize <- randomRIO (2, 5) -- Tournament size between 2 and 5
            tournament <- V.replicateM tournamentSize $ do
                index <- randomRIO (0, V.length pop - 1)
                return $ pop V.! index
            return $ V.minimumBy compareIndividuals tournament -- TOOD: Minimization assumption

        crossoverInds :: Individual -> Individual -> (Genome -> Maybe (Double, Int)) -> IO (Individual, Individual)
        crossoverInds p1@(Individual (g1, _)) p2@(Individual (g2, _)) eval = do
            p <- randomRIO (0, 1 :: Double)
            if p < xRate
                then do
                    (c1, c2) <- crossover g1 g2
                    pure (genomeToIndividual eval c1, genomeToIndividual eval c2)
                else return (p1, p2)

        mutateInd :: Individual -> (Genome -> Maybe (Double, Int)) -> IO Individual
        mutateInd it@(Individual (g, _)) eval = do
            p <- randomRIO (0, 1 :: Double)
            if p < mutRate
                then do
                    g' <- mutate g codonMax
                    return $ Individual (g', eval g')
                else return it

        pruneInd :: Individual -> IO Individual
        pruneInd it@(Individual (_, Nothing)) = return it
        pruneInd it@(Individual (g, Just (f, n))) = do
            p <- randomRIO (0, 1 :: Double)
            return $ if p < pruneRate
                then Individual (prune g n, Just (f, n))
                else it

sortPopulation :: V.Vector Individual -> V.Vector Individual
sortPopulation pop = V.create $ do
        asMutable <- V.thaw pop
        VA.sortBy compareIndividuals asMutable
        return asMutable

compareIndividuals :: Individual -> Individual -> Ordering
compareIndividuals (Individual (_, j1)) (Individual (_, j2)) = compareGenomeFitness j1 j2

-- TODO: Here we are forcing minimizing fitnesses!
compareGenomeFitness :: Maybe (Double, Int) -> Maybe (Double, Int) -> Ordering
compareGenomeFitness (Just (f1, _)) (Just (f2, _)) = compare f1 f2
compareGenomeFitness Nothing        (Just _)       = GT
compareGenomeFitness (Just _)       Nothing        = LT
compareGenomeFitness _              _              = EQ

calculatePopStatistics :: Int -> (Genome -> Text) -> V.Vector Individual -> GenerationStatistics
calculatePopStatistics gen prtr is = GS {
                                generation = gen,

                                fitnessAvg = fitAvg,
                                fitnessStdDev = fitStddev,

                                localBest = prtr $ repr best,
                                localBestInd = best,
                                localBestFit = fromMaybe (-1) (fitness best),

                                populationSize = V.length is,
                                invalidInds = invalid,

                                genomeSizeAvg = genSizeAvg,
                                effectiveGenomeSizeAvg = effSizeAvg,
                                top10PercEffectiveGenomeSizeAvg = top10EffSizeAvg,
                                top10PercFitnessAvg = top10FitAvg,
                                top10PercFitnessStdDev = top10FitStdDev
                            }
    where
        -- TODO: This can be massively optimized...

        -- Extract valid individuals (those with a fitness value)
        validInds = filterBad is

        -- Extract fitness values
        fitnessVals = V.map (\(Individual (_, Just (f, _))) -> f) validInds

        -- Best individual by fitness
        best = V.minimumBy compareIndividuals validInds

        -- Average fitness
        fitAvg = if V.null fitnessVals
                    then 1/0 -- TODO: This is a minimum fitness assumption
                    else V.sum fitnessVals / fromIntegral (V.length fitnessVals)

        -- Fitness standard deviation
        fitStddev = if V.null fitnessVals
            then 1/0 -- TODO: This is a minimum fitness assumption
            else sqrt $ V.sum (V.map (\f -> (f - fitAvg) ** 2) fitnessVals) / fromIntegral (V.length fitnessVals)

        -- Count of invalid individuals
        invalid = V.length is - V.length validInds

        -- Extract genome sizes
        (totGenSz, totEffGenSz) = if V.null fitnessVals
                        then (-1 :: Int, -1 :: Int)
                        else V.foldr' (\(Individual (g, Just (_, s))) (totSz, totEffSz) -> (totSz + G.size g, totEffSz + s)) (0, 0) validInds -- V.map (\(Individual (_, Just (_, g))) -> g) validInds

        -- Average genome size
        genSizeAvg = if V.null validInds
                    then -1
                    else fromIntegral totGenSz / fromIntegral (V.length validInds)

        -- Average effective genome size
        effSizeAvg = if V.null validInds
                     then -1
                     else fromIntegral totEffGenSz / fromIntegral (V.length validInds)

        top10Perc = V.take (V.length is `div` 10) validInds

        top10EffSizeAvg = if V.null top10Perc
                          then -1 -- TODO: Code clean up; this calculates the effective genome size of the top 10% of the population
                          else fromIntegral (V.foldr' (\(Individual (_, Just (_, s))) acc -> acc + s) (0 :: Int) top10Perc) / fromIntegral (V.length top10Perc)

        top10FitAvg = if V.null top10Perc
                      then 1/0
                      else V.foldr' (\(Individual (_, Just (f, _))) acc -> acc + f) 0.0 top10Perc / fromIntegral (V.length top10Perc)

        top10FitStdDev = if V.null top10Perc
                         then 1/0
                         else V.foldr' (\(Individual (_, Just (f, _))) acc -> acc + ((f - top10FitAvg) ** 2)) 0.0 top10Perc / fromIntegral (V.length top10Perc)


generateRandomGenome :: Int -> Int -> IO Genome
generateRandomGenome len codonMax = Genome <$> V.replicateM len (randomRIO (0, codonMax))

generateGenomePopulation :: Int -> Int -> Int -> IO (V.Vector Genome)
generateGenomePopulation n genomeLen codonMax = V.replicateM n (generateRandomGenome genomeLen codonMax)

crossover :: Genome -> Genome -> IO (Genome, Genome)
crossover (Genome p1) (Genome p2) = do
    r <- randomRIO (0, min (V.length p1) (V.length p2) - 1)
    let c1 = Genome $ V.slice 0 r p1 V.++ V.slice r (V.length p2 - r) p2
    let c2 = Genome $ V.slice 0 r p2 V.++ V.slice r (V.length p1 - r) p1
    return (c1, c2)

mutate :: Genome -> Int -> IO Genome
mutate (Genome g) codonMax = do
    i <- randomRIO (0, V.length g - 1)
    codon <- randomRIO (0, codonMax)
    return $ Genome $ g V.// [(i, codon)]

prune :: Genome -> Int -> Genome
prune (Genome g) n = Genome $ V.take (n + 1) g -- TODO: Is this +1 or not?
