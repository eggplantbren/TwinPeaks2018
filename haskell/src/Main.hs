{-# LANGUAGE BangPatterns #-}

module Main where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text.IO as TIO
import System.IO
import System.Random.MWC
import TwinPeaks2018.Model

-- Main action obv.
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    -- Print a message
    putStr "Doing 1000000 MCMC steps..."
    hFlush stdout

    -- Open output file
    h <- openFile "output/explorePrior.csv" WriteMode

    -- Do some MCMC exploration of the prior of the trivial example
    let model = trivialExampleModel

    TIO.hPutStrLn h $ header model

    -- Initial position
    particle <- fromPrior model rng

    -- The MCMC loop
    _ <- explorePrior 1000000 particle model h rng

    putStrLn "done."

    return ()


explorePrior :: Int                   -- Number of steps to do
             -> a                     -- Initial state
             -> Model a               -- Model specification
             -> Handle                -- Handle to output file
             -> Gen RealWorld         -- RNG
             -> IO a                  -- Updated state
explorePrior !steps particle model h rng
    | steps <= 0 = do
                     TIO.hPutStrLn h $ render model particle
                     return particle
    | otherwise  = loop
      where loop = do
                     TIO.hPutStrLn h $ render model particle
                     (proposal, logH) <- perturb model particle rng
                     u <- uniform rng :: IO Double
                     let particle' = if u < exp logH then proposal else particle
                     explorePrior (steps-1) particle' model h rng

