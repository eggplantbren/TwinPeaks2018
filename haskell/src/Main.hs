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
    let model = trivialExampleModel

    TIO.putStrLn $ header model

    -- Initial position
    particle <- fromPrior model rng

    -- The MCMC loop
    _ <- explorePrior 1000 particle model rng

    return ()


explorePrior :: Int                   -- Number of steps to do
             -> a                     -- Initial state
             -> Model a               -- Model specification
             -> Gen RealWorld         -- RNG
             -> IO a                  -- Updated state
explorePrior !steps particle model rng
    | steps <= 0 = do
                     TIO.putStrLn $ render model particle
                     return particle
    | otherwise  = loop
      where loop = do
                     TIO.putStrLn $ render model particle
                     (proposal, logH) <- perturb model particle rng
                     u <- uniform rng :: IO Double
                     let particle' = if u < exp logH then proposal else particle
                     explorePrior (steps-1) particle' model rng

