{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module EvoAlg.TreeGenetic.Evolution (
    evolve, GenerationStatistics(..)
) where

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA

import System.Random (randomRIO)

import Data.Maybe (isJust, fromMaybe)
import Text.Printf (printf)
import Data.Text (Text)

import Lang.Math
import qualified Lang.Math as E

newtype Individual = Individual (Expr, Maybe (Double, Int))
    deriving (Eq, Show)

repr :: Individual -> Expr
repr (Individual (r, _)) = r

depth :: Individual -> Maybe Int
depth (Individual (_, Just (_, n))) = Just n
depth _                             = Nothing

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

    exprDepthAvg :: Double,

    top10PercExprDepthAvg :: Double,
    top10PercFitnessAvg :: Double,
    top10PercFitnessStdDev :: Double
}
    deriving (Eq, Show)

filterBad :: V.Vector Individual -> V.Vector Individual
filterBad = V.filter (\(Individual (_, f)) -> isJust f)

dashes :: IO ()
dashes = putStrLn $ replicate 120 '-'

exprToIndividual :: (Expr -> Maybe (Double, Int)) -> Expr -> Individual
exprToIndividual f g = Individual (g, f g)

freshPopulation :: Int -> Int -> (Expr -> Maybe (Double, Int)) -> IO (V.Vector Individual)
freshPopulation popGen maxDepth indEval = V.map (exprToIndividual indEval) <$> generateExprPopulation popGen maxDepth

evolve :: Int -> Int -> Int -> Double -> Double -> Double -> (Expr -> Maybe (Double, Int)) -> (Expr -> Text) -> IO (V.Vector Individual, [GenerationStatistics])
evolve _ _ 0 _ _ _ _ _ = pure (V.empty, []) -- No generations allowed
evolve popGen maxExprDepth maxGens mutRate xRate pruneRate indEval indPrtr = do
    -- Generate the initial population
    initialPop <- sortPopulation <$> freshPopulation popGen maxExprDepth indEval
    let initialStats = calculatePopStatistics 0 indPrtr initialPop

    -- Print configuration
    dashes
    putStrLn "Grammatical Evolution"
    dashes
    putStrLn "Configuration:"
    printf "    Population Size: %d\n" popGen
    printf "    Max Generations: %d\n" maxGens
    printf "    Max Expr Depth: %d\n" maxExprDepth
    printf "    Mutation Rate: %.f%%\n" (mutRate * 100)
    printf "    Crossover Rate: %.f%%\n" (xRate * 100)
    printf "    Prune Rate (UNUSED): %.f%%\n" (pruneRate * 100)
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
                printf "| Gen %d | Avg Fitness: %.2f | StdDev: %.2f | Pop Size: %d | Best Fitness: %.2f | Best Depth: %d | %s\n"
                    gen
                    (fitnessAvg nextStats)
                    (fitnessStdDev nextStats)
                    (V.length nextPop)
                    (fromMaybe 0.0 (fitness (localBestInd nextStats)))
                    ((\(Individual (e, _)) -> E.depth e) (localBestInd nextStats))
                    (localBest nextStats)

                -- Continue to the next generation
                evolve' nextPop (nextStats : stats) (gen + 1)

        nextGeneration :: V.Vector Individual -> IO (V.Vector Individual)
        nextGeneration start = do
            -- let goodPop = filterBad start

            let elites = V.take (popGen `div` 6) start -- V.take eliteSize goodPop
            freshPop <- freshPopulation (popGen `div` 6) maxExprDepth indEval

            -- TODO: Inlining crossover here because crossover is a bit different for TGP (i.e., one child)
            let remainderN = popGen - V.length elites - V.length freshPop
            let offspringN = remainderN - round (xRate * fromIntegral remainderN)
            let tournamN   = remainderN - offspringN

            offspring <- V.replicateM offspringN $ do
                (Individual (p1, _)) <- selectParent start -- pulling from whole population
                (Individual (p2, _)) <- selectParent start -- pulling from whole population
                c <- exprToIndividual indEval <$> crossover p1 p2 maxExprDepth
                mutateInd c indEval

                -- cs <- crossoverInds p1 p2 indEval
                -- mapM (`mutateInd` indEval) cs -- >>= pruneInd

            remainder <- V.replicateM tournamN $ do
                t <- selectParent start
                mutateInd t indEval

            return $ sortPopulation $ V.concat [elites, offspring, remainder, freshPop]
            -- return $ sortPopulation $ elites V.++ offspring V.++ remainder V.++ freshPop

        selectParent :: V.Vector Individual -> IO Individual
        selectParent pop = do
            tournamentSize <- randomRIO (2, 5) -- Tournament size between 2 and 5
            tournament <- V.replicateM tournamentSize $ do
                index <- randomRIO (0, V.length pop - 1)
                return $ pop V.! index
            return $ V.minimumBy compareIndividuals tournament -- TOOD: Minimization assumption

        crossoverInds :: Individual -> Individual -> (Expr -> Maybe (Double, Int)) -> IO [Individual]
        crossoverInds p1@(Individual (g1, _)) p2@(Individual (g2, _)) eval = do
            p <- randomRIO (0, 1 :: Double)
            if p < xRate
                then do
                    c <- crossover g1 g2 maxExprDepth
                    pure [exprToIndividual eval c]
                else return [p1, p2]

        mutateInd :: Individual -> (Expr -> Maybe (Double, Int)) -> IO Individual
        mutateInd it@(Individual (g, _)) eval = do
            p <- randomRIO (0, 1 :: Double)
            if p < mutRate
                then do
                    g' <- mutate g maxExprDepth
                    return $ Individual (g', eval g')
                else return it

        -- pruneInd :: Individual -> IO Individual
        -- pruneInd it@(Individual (_, Nothing)) = return it
        -- pruneInd it@(Individual (g, Just (f, n))) = do
        --     p <- randomRIO (0, 1 :: Double)
        --     return $ if p < pruneRate
        --         then Individual (prune g n, Just (f, n))
        --         else it

sortPopulation :: V.Vector Individual -> V.Vector Individual
sortPopulation pop = V.create $ do
        asMutable <- V.thaw pop
        VA.sortBy compareIndividuals asMutable
        return asMutable

compareIndividuals :: Individual -> Individual -> Ordering
compareIndividuals (Individual (_, j1)) (Individual (_, j2)) = compareExprFitness j1 j2

-- TODO: Here we are forcing minimizing fitnesses!
compareExprFitness :: Maybe (Double, Int) -> Maybe (Double, Int) -> Ordering
compareExprFitness (Just (f1, _)) (Just (f2, _)) = compare f1 f2
compareExprFitness Nothing        (Just _)       = GT
compareExprFitness (Just _)       Nothing        = LT
compareExprFitness _              _              = EQ

calculatePopStatistics :: Int -> (Expr -> Text) -> V.Vector Individual -> GenerationStatistics
calculatePopStatistics gen prtr is = GS {
                                        generation = gen,

                                        fitnessAvg = fitAvg,
                                        fitnessStdDev = fitStddev,

                                        localBest = prtr $ repr best,
                                        localBestInd = best,
                                        localBestFit = fromMaybe (-1) (fitness best),

                                        populationSize = V.length is,
                                        invalidInds = invalid,

                                        exprDepthAvg = depthAvg,
                                        top10PercExprDepthAvg = top10EffSizeAvg,
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

        -- Average genome size
        depthAvg = if V.null validInds
                    then 0.0
                    else fromIntegral (V.foldr' (\(Individual (_, Just (_, s))) acc -> acc + s) 0 validInds) / fromIntegral (V.length validInds)

        top10Perc = V.take (V.length is `div` 10) validInds

        top10EffSizeAvg = if V.null top10Perc
                          then 0.0 -- TODO: Code clean up; this calculates the effective genome size of the top 10% of the population
                          else fromIntegral (V.foldr' (\(Individual (_, Just (_, s))) acc -> acc + s) (0 :: Int) top10Perc) / fromIntegral (V.length top10Perc)

        top10FitAvg = if V.null top10Perc
                      then 1/0
                      else V.foldr' (\(Individual (_, Just (f, _))) acc -> acc + f) 0.0 top10Perc / fromIntegral (V.length top10Perc)

        top10FitStdDev = if V.null top10Perc
                         then 1/0
                         else V.foldr' (\(Individual (_, Just (f, _))) acc -> acc + ((f - top10FitAvg) ** 2)) 0.0 top10Perc / fromIntegral (V.length top10Perc)

generateRandomExpr :: Int -> IO Expr
generateRandomExpr maxDepth = do
    now <- randomRIO (1, maxDepth)
    generateRandomExprOfDepth now

generateRandomExprOfDepth :: Int -> IO Expr
generateRandomExprOfDepth n
    | n <= 1 = return X
    | otherwise = do
        con <- randomRIO (0, 1 :: Int)
        if con == 0
            then randomUnOp n
            else randomBinOp n

randomUnOp :: Int -> IO Expr
randomUnOp n
    | n <= 1 = error "Invalid n for randomUnOp"
    | otherwise = do
        op <- randomRIO (0, 4 :: Int)
        let op' = case op of
                    0 -> Neg
                    1 -> Abs
                    2 -> Sin
                    3 -> Cos
                    4 -> Tan
                    _ -> error "Invalid case randomUnOp inner"
        subExpr <- generateRandomExprOfDepth (n - 1)
        return $ UnOp op' subExpr

randomBinOp :: Int -> IO Expr
randomBinOp n
    | n <= 1 = error "Invalid n for randomBinOp"
    | otherwise = do
        op <- randomRIO (0, 4 :: Int)
        let op' = case op of
                    0 -> Add
                    1 -> Sub
                    2 -> Mul
                    3 -> Div
                    4 -> Rem
                    _ -> error "Invalid case randomBinOp inner"
        subLExpr <- generateRandomExprOfDepth (n - 1)
        subRExpr <- generateRandomExprOfDepth (n - 1)
        return $ BinOp op' subLExpr subRExpr

generateExprPopulation :: Int -> Int -> IO (V.Vector Expr)
generateExprPopulation n maxDepth = V.replicateM n (generateRandomExpr maxDepth)

crossover :: Expr -> Expr -> Int -> IO Expr
crossover l r maxDepth = do
    donor <- randomRIO (0, 1 :: Int)
    let (donorE, receiverE) = if donor == 0 then (l, r) else (r, l)
    donorAtMaxDepth <- randomRIO (0, maxDepth - E.depth donorE)
    donorSubTree <- pickRandomSubTree donorE donorAtMaxDepth
    
    -- Make sure that we're placing the subtree at a depth that doesn't allow us
    -- to build expressions that are too large
    placeAtMaxDepth <- randomRIO (0, maxDepth - E.depth donorSubTree)
    placeSubTreeRandomly receiverE donorSubTree placeAtMaxDepth

    where
        pickRandomSubTree :: Expr -> Int -> IO Expr
        pickRandomSubTree X _ = return X
        pickRandomSubTree it@(UnOp _ sub) i
            | i <= 1 = return it
            | otherwise = pickRandomSubTree sub (i - 1)
        pickRandomSubTree it@(BinOp _ subl subr) i
            | i <= 1 = return it
            | otherwise = do
                dir <- randomRIO (0, 1 :: Int)
                let trg = if dir == 0 then subl else subr
                pickRandomSubTree trg (i - 1)
        
        placeSubTreeRandomly :: Expr -> Expr -> Int -> IO Expr
        placeSubTreeRandomly X donor _ = return donor
        placeSubTreeRandomly (UnOp op sub) donor i
            | i <= 1 = return donor
            | otherwise = do
                sub' <- placeSubTreeRandomly sub donor (i-1)
                return $ UnOp op sub'
        placeSubTreeRandomly (BinOp op subl subr) donor i
            | i <= 1 = return donor
            | otherwise = do
                dir <- randomRIO (0, 1 :: Int)
                new <- placeSubTreeRandomly (if dir == 0 then subl else subr) donor (i-1)
                return $ if dir == 0
                    then BinOp op new subr
                    else BinOp op subl new


mutate :: Expr -> Int -> IO Expr
mutate e maxDepth = do
    maxHops <- randomRIO (1, maxDepth - E.depth e)
    mutate' e maxHops 0
    where
        mutate' :: Expr -> Int -> Int -> IO Expr
        mutate' trg n h
            | n <= 1 = generateRandomExprOfDepth (maxDepth - h)
            | otherwise = case trg of
                X -> generateRandomExpr (maxDepth - n - h)
                (UnOp op sub) -> do
                    sub' <- mutate' sub (n-1) (h+1)
                    return $ UnOp op sub'
                (BinOp op subl subr) -> do
                    lr <- randomRIO (0, 1 :: Int)
                    new <- mutate' (if lr == 0 then subl else subr) (n-1) (h+1)
                    return $ if lr == 0
                        then BinOp op new subr
                        else BinOp op subl new
