module Main where

-- Imports
import Control.Monad.Trans.Maybe
import qualified Data.Text.IO as TIO
import System.Random.MWC
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
        Just s  -> TIO.putStrLn $ samplerStateToText s
    return ()

