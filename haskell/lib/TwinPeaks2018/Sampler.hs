{-
    Types and functions to do with the TwinPeaks2018 algorithm.
-}

{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Sampler (generateSampler,
                              samplerStateToText,
                              SamplerState(..)) where

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
                          shadowParticles :: !(Particles a),
                          nsParticleUccs  :: !(V.Vector Scalar)
                      }


-- FUNCTIONS AND ACTIONS ---------------------------

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
    let fs = V.zip fEvals us1
    let gs = V.zip gEvals us2

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

        -- Partially applied function
        let rawUcc fVal gVal = particleUcc fVal gVal shadowParticles
        let rawUccs = V.zipWith rawUcc (fs nsParticles) (gs nsParticles)

        -- Tiebreaker values for the UCCs
        tieBreakers <- V.replicateM numParticles (uniform rng :: IO Double)
        let nsParticleUccs = V.zip rawUccs tieBreakers

        putStrLn "done."
        return $! SamplerState {..}


-- Evaluating this function computes the raw UCC at a given position
particleUcc :: Scalar
            -> Scalar
            -> Particles a          -- The shadow particles
            -> Double
particleUcc f g (Particles _ fs gs) =
    let
        -- Whether each of the fs and gs is in
        -- the upper right rectangle of (f, g)
        upperRight :: V.Vector Bool
        upperRight = V.zipWith (\f' g' -> (f' > f && g' > g)) fs gs

        toDouble :: Bool -> Double
        toDouble b = if b then 1.0 else 0.0
    in
        V.foldl' (\acc u -> acc + toDouble u) 0.0 upperRight
        


-- Nice text output of a SamplerState object
samplerStateToText :: SamplerState a -> T.Text
samplerStateToText SamplerState {..} =
    let
        -- NS particles first
        fs' = V.map fst (fs nsParticles)  -- Remove tiebreakers
        gs' = V.map fst (gs nsParticles)  -- Remove tiebreakers

        -- Shadow particles
        fs'' = V.map fst (fs shadowParticles)
        gs'' = V.map fst (gs shadowParticles)

        -- Combined
        fsAll = V.concat [fs', fs'']
        gsAll = V.concat [gs', gs'']

        -- Pad UCCs with -1s for the shadow particles
        rawUccsAll = V.concat [V.map fst nsParticleUccs,
                               V.replicate (V.length nsParticleUccs) (-1.0)]

        -- As triples
        triples = V.zip3 fsAll gsAll rawUccsAll

        -- Triple -> Text
        toText (x, y, z) = T.pack $ show x ++ "," ++
                                    show y ++ "," ++
                                    (if z==(-1.0) then "NA" else show z) ++ "\n"
    in
        T.concat . V.toList $ V.map toText triples


