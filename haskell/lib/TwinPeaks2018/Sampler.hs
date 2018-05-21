{-
    Types and functions to do with the TwinPeaks2018 algorithm.
-}

{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Sampler (generateSampler,
                              samplerStateToText) where

-- Imports
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import System.IO
import System.Random.MWC
import TwinPeaks2018.Model
import TwinPeaks2018.Scalar


-- TYPES ------------------------

-- Represents a collection of particles and their metadata
data Particles a = Particles
                   {
                       particles :: !(V.Vector a),
                       fs        :: !(V.Vector Scalar),
                       gs        :: !(V.Vector Scalar)
                   }


-- Represents the state of the sampler,
-- with two collections of particles
data SamplerState a = SamplerState
                      {
                          nsParticles     :: !(Particles a),
                          shadowParticles :: !(Particles a)
                      }


-- Generate a bunch of particles from the prior
generateParticles :: Int
                  -> Model a
                  -> Gen RealWorld
                  -> IO (Particles a)
generateParticles numParticles Model {..} rng = do

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

    return $! Particles {..}


-- Generate an initial SamplerState.
generateSampler :: Int
                -> Model a
                -> Gen RealWorld
                -> MaybeT IO (SamplerState a)
generateSampler numParticles Model {..} rng
    | numParticles <= 0 = MaybeT . return $ Nothing
    | otherwise         = lift $ do

        -- Create and print message
        let message = "Generating initial sampler state\nwith "
                         ++ show numParticles ++ " NS particles and "
                         ++ show numParticles ++ " shadow particles..."
        putStr message
        hFlush stdout

        nsParticles     <- generateParticles numParticles Model {..} rng
        shadowParticles <- generateParticles numParticles Model {..} rng

        putStrLn "done."
        return $! SamplerState {..}


-- Nice text output of a Particles object
particlesToText :: Particles a -> T.Text
particlesToText Particles {..} =
    let
        fs' = V.map fst fs  -- Remove tiebreakers
        gs' = V.map fst gs  -- Remove tiebreakers
        scalars = V.toList $ V.zip fs' gs' -- Convert to pairs

        -- Pair -> Text
        toText (x, y) = T.pack $ show x ++ "," ++ show y ++ "\n"
    in
        T.concat $ map toText scalars



-- Nice text output of a SamplerState object
samplerStateToText :: SamplerState a -> T.Text
samplerStateToText SamplerState {..} = T.append text1 text2 where
    text1 = particlesToText nsParticles
    text2 = particlesToText shadowParticles

