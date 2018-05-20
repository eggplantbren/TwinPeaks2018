{-
    Types and functions to do with the TwinPeaks2018 algorithm.
-}

{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Sampler where

-- Imports
import Control.Monad.Primitive
import qualified Data.Vector as V
import System.IO
import System.Random.MWC
import TwinPeaks2018.Model
import TwinPeaks2018.Scalar

-- Represents particles and their metadata
data Particles a = Particles
                   {
                       particles :: !(V.Vector a),
                       fs        :: !(V.Vector Scalar),
                       gs        :: !(V.Vector Scalar)
                   }


-- Generate a bunch of particles from the prior
generateParticles :: Int
                  -> Model a
                  -> Gen RealWorld -> IO (Particles a)
generateParticles numParticles Model {..} rng = do

    -- Create and print message
    let message = "Generating " ++ show numParticles ++ " particles "
                      ++ "from the prior..."
    putStr message
    hFlush stdout

    -- Generate the particles and evaluate the scalars
    particles <- V.replicateM numParticles (fromPrior rng)
    let fEvals = V.map f particles
    let gEvals = V.map g particles

    -- Need tiebreakers to pair with the scalars
    us1 <- V.replicateM numParticles (uniform rng :: IO Double)
    us2 <- V.replicateM numParticles (uniform rng :: IO Double)

    -- Do the pairing
    let fs = V.zipWith (,) fEvals us1
    let gs = V.zipWith (,) gEvals us2

    putStrLn "done."

    return $! Particles {..}

