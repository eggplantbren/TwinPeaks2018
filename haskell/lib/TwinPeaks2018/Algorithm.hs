{-
    Types and functions to do with the TwinPeaks2018 algorithm.
    I wanted more than just Sampler.hs. This has the functions
    that update a SamplerState, and so on.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Algorithm (update) where

-- Imports
import Control.Monad
import Control.Monad.Primitive
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.IO
import System.Random.MWC
import TwinPeaks2018.Model
import TwinPeaks2018.Sampler
import TwinPeaks2018.Utils

-- Find the index of the worst particle
findWorst :: SamplerState a -> Int
findWorst SamplerState {..} = V.maxIndex nsParticleUccs

-- One iteration of Nested Sampling
update :: SamplerState a
       -> Gen RealWorld
       -> IO (SamplerState a)
update sampler@SamplerState {..} rng = do

    -- Update iteration number, print message
    let iteration' = iteration + 1
    putStrLn $ "Iteration " ++ show iteration' ++ "..."
    hFlush stdout

    -- Which particle to kill
    let iKill = findWorst sampler
    let  kill = nsParticles V.! iKill

    -- Open the output file
    h <- openFile "output/output.csv" $ if iteration==0
                                        then WriteMode
                                        else AppendMode
    -- Write CSV header
    when (iteration == 0) $ TIO.hPutStrLn h (header theModel)

    -- Write that particle to disk
    saveParticle kill theModel h

    -- Choose copy to replace it with
    iCopy <- chooseCopy iKill numParticles rng

    -- Generate replacement particle
    replacement <- refresh 1000 (nsParticles V.! iCopy)
                                theModel rng

    -- Close output file
    hClose h
    putStrLn "done.\n"

    return $! sampler



-- Write a particle to disk
saveParticle :: Particle a
             -> Model a
             -> Handle
             -> IO ()
saveParticle Particle {..} Model {..} h = TIO.hPutStrLn h $ render coords


-- Refresh a particle
refresh :: Int
        -> Particle a
        -> Model a
        -> Gen RealWorld
        -> IO (Particle a)
refresh steps !particle theModel rng
    | steps == 0 = return particle
    | otherwise  = do
                     refresh (steps-1) particle theModel rng
