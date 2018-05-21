module Main where

-- Imports
--import Control.Monad.Primitive
import qualified Data.Text.IO as TIO
--import System.IO
import System.Random.MWC
import TwinPeaks2018.Sampler
import TwinPeaks2018.Model

-- Main action obv.
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    -- Generate a set of particles.
    someParticles <- generateParticles 1000 trivialExampleModel rng
    TIO.putStrLn $ particlesToText someParticles

    return ()

