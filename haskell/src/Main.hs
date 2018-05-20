{-# LANGUAGE BangPatterns #-}

module Main where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text.IO as TIO
import System.Random.MWC
import TwinPeaks2018.Model

-- Main action obv.
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    -- Do some MCMC exploration of the prior of the trivial example
    let steps = 10000
    let model = trivialExampleModel

    TIO.putStrLn $ header model

    -- Initial position
    x <- fromPrior model rng
    TIO.putStrLn $ render model x

    -- The MCMC loop
    _ <- priorSteps steps x model rng

    return ()


priorSteps :: Int                   -- Number of steps to do
           -> a                     -- Initial state
           -> Model a               -- Model specification
           -> Gen RealWorld         -- RNG
           -> IO a                  -- Updated state
priorSteps !k particle model rng
    | k <= 0    = return particle
    | otherwise = loop
    where loop = do
                     (particle', _) <- perturb model particle rng
                     TIO.putStrLn $ render model particle
                     priorSteps (k-1) particle' model rng

