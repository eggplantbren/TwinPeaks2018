module Main where

-- Imports
import Control.Monad
import Control.Monad.Trans.Maybe
import System.Random.MWC
import TwinPeaks2018.Algorithm
import TwinPeaks2018.Sampler
import TwinPeaks2018.Model

-- Main action obv.
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    -- Generate a Maybe Sampler
    maybeSampler <- runMaybeT $ generateSampler 1000 trivialExampleModel rng

    -- If it succeeded, print particle scalars
    case maybeSampler of
        Nothing -> putStrLn "Error creating sampler."
        Just s  -> do
                     _ <- nestedSampling 2000 s rng
                     return ()
    return ()

