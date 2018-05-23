{-
    Types and functions to do with the TwinPeaks2018 algorithm.
-}

{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Sampler (generateSampler,
                              samplerStateToText,
                              Particle(..),
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

-- Represents a single particle and its metadata
data Particle a = Particle
                  {
                      coords :: !a,
                      fValue :: !Scalar,
                      gValue :: !Scalar
                  }

-- A collection of particles
type Particles a = V.Vector (Particle a)

-- Represents the state of the sampler,
-- with two collections of particles
data SamplerState a = SamplerState
                      {
                          theModel        :: !(Model a),
                          iteration       :: !Int,
                          numParticles    :: !Int,
                          nsParticles     :: !(Particles a),
                          shadowParticles :: !(Particles a),
                          nsParticleUccs  :: !(V.Vector Scalar)
                      }


-- FUNCTIONS AND ACTIONS ---------------------------

-- Extraction of scalars from a collection of particles
getFValues :: Particles a
           -> V.Vector Scalar
getFValues = V.map (\(Particle _ f _ ) -> f)

getGValues :: Particles a
           -> V.Vector Scalar
getGValues = V.map (\(Particle _ _ g ) -> g)


-- Generate a bunch of particles from the prior
generateParticles :: Int
                  -> Model a
                  -> Gen RealWorld
                  -> IO (Particles a)
generateParticles num Model {..} rng = do

    -- Generate the coordinates and evaluate the scalars
    coords <- V.replicateM num (fromPrior rng)
    let fEvals = V.map f coords
    let gEvals = V.map g coords

    -- Need tiebreakers to pair with the scalars
    us1 <- V.replicateM num (uniform rng :: IO Double)
    us2 <- V.replicateM num (uniform rng :: IO Double)

    -- Do the pairing
    let fs = V.zip fEvals us1
    let gs = V.zip gEvals us2

    return $! V.zipWith3 (\c fVal gVal -> Particle c fVal gVal)
                         coords fs gs



-- Evaluating this function computes the raw UCC at a given position
particleUcc :: Scalar
            -> Scalar
            -> Particles a          -- The shadow particles
            -> Double
particleUcc f g shadowParticles =
    let
        -- Grab f and g values of shadow particles
        fs = getFValues shadowParticles
        gs = getGValues shadowParticles

        -- Whether each of the fs and gs is in
        -- the upper right rectangle of (f, g)
        upperRight :: V.Vector Bool
        upperRight = V.zipWith (\f' g' -> (f' > f && g' > g)) fs gs

        toDouble :: Bool -> Double
        toDouble b = if b then 1.0 else 0.0
    in
        V.foldl' (\acc u -> acc + toDouble u) 0.0 upperRight



-- Generate an initial SamplerState.
generateSampler :: Int
                -> Model a
                -> Gen RealWorld
                -> MaybeT IO (SamplerState a)
generateSampler numParticles Model {..} rng
    | numParticles <= 0 = MaybeT . return $ Nothing
    | otherwise         = lift $ do

        -- Create and print message
        putStrLn "Generating initial sampler state..."
        putStr $ "    Generating "
                         ++ show numParticles ++ " NS particles and "
                         ++ show numParticles ++ " shadow particles..."
        hFlush stdout

        let iteration = 0
        let theModel = Model {..}

        nsParticles     <- generateParticles numParticles Model {..} rng
        shadowParticles <- generateParticles numParticles Model {..} rng

        -- Partially applied function
        let rawUcc fVal gVal = particleUcc fVal gVal shadowParticles

        -- Grab f and g values of shadow particles
        let fs = getFValues shadowParticles
        let gs = getGValues shadowParticles
        let rawUccs = V.zipWith rawUcc fs gs

        -- Tiebreaker values for the UCCs
        tieBreakers <- V.replicateM numParticles (uniform rng :: IO Double)
        let nsParticleUccs = V.zip rawUccs tieBreakers

        putStrLn "done."
        putStrLn "done.\n"
        return $! SamplerState {..}




-- Nice text output of a SamplerState object
samplerStateToText :: SamplerState a -> T.Text
samplerStateToText SamplerState {..} =
    let
        -- NS particles first
        fs' = V.map fst (getFValues nsParticles)  -- Remove tiebreakers
        gs' = V.map fst (getGValues nsParticles)  -- Remove tiebreakers

        -- Shadow particles
        fs'' = V.map fst (getFValues shadowParticles)
        gs'' = V.map fst (getGValues shadowParticles)

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


