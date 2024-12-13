module EvoAlg.Grammatical (
    -- * Evolution
    generateRandomGenome, generateGenomePopulation,
    crossover, mutate, evolve,
    Individual, repr, necessaryGenes, fitness,
    GenerationStatistics(..),

    -- * Genome
    Genome(..),
    mkGenome, mkGenomeL,
    genomeToText, size
) where

import EvoAlg.Grammatical.Genome

import EvoAlg.Grammatical.Evolution
