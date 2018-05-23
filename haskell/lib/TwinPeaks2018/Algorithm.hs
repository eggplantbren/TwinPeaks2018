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
--import qualified Data.Vector.Mutable as VM
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
    putStr "    Doing MCMC..."
    hFlush stdout
    replacement <- refresh 10000 iKill (nsParticles V.! iCopy) sampler rng
    putStrLn "done."

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


-- Refresh a particle using 'steps' metropolis steps
refresh :: Int
        -> Int
        -> Particle a
        -> SamplerState a
        -> Gen RealWorld
        -> IO (Particle a)
refresh steps iKill !particle@(Particle coords fValue gValue uccTb)
        SamplerState {..} rng
    | steps == 0 = return particle
    | otherwise  = do
                     -- Single ' denotes proposed
                     -- Double '' denotes actual new state
                     (coords', logH) <- perturb theModel coords rng
                     u <- uniform rng :: IO Double
                     if u >= exp logH then return particle -- Pre-reject
                     else do
                            -- Getter for the UCC tiebreaker of a particle
                            let getUccTiebreaker (Particle _ _ _ t) = t

                            -- Tiebreaker proposals
                            tbf' <- trivialExampleWrap . (snd fValue + )
                                                    <$> randh rng
                            tbg' <- trivialExampleWrap . (snd gValue + )
                                                    <$> randh rng
                            tbucc' <- trivialExampleWrap . (uccTb + )
                                                    <$> randh rng
                            let fValue' = (f theModel coords', tbf')
                            let gValue' = (g theModel coords', tbg')
                            let ucc' = particleUcc
                                           fValue' gValue' shadowParticles

                            let uccPair' = (ucc', tbucc')
                            let threshold = (nsParticleUccs V.! iKill,
                                                getUccTiebreaker $
                                                    nsParticles V.! iKill)

                            let particle'' = if uccPair' < threshold
                                             then Particle coords'
                                                       fValue' gValue' tbucc'
                                             else particle

                            refresh (steps-1) iKill particle''
                                        SamplerState {..} rng

