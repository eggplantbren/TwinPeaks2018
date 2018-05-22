{-
    Types and functions to do with the TwinPeaks2018 algorithm.
    I wanted more than just Sampler.hs. This has the functions
    that update a SamplerState, and so on.
-}

{-# LANGUAGE RecordWildCards #-}

module TwinPeaks2018.Algorithm where

-- Imports
import qualified Data.Vector as V
import TwinPeaks2018.Sampler

-- Find the index of the worst particle
findWorst :: SamplerState a -> Int
findWorst SamplerState {..} = V.maxIndex nsParticleUccs
