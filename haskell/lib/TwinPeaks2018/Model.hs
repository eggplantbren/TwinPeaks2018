{-
    Define models to work with.
-}

module TwinPeaks2018.Model where

-- Imports
import Control.Monad.Primitive
import qualified Data.Text as T
import System.Random.MWC
import TwinPeaks2018.Utils

-- a is the hypothesis space
data Model a = Model
               {
                   -- Generate from the prior
                   fromPrior :: Gen RealWorld -> IO a,

                   -- Metropolis proposal
                   perturb   :: a -> Gen RealWorld -> IO (a, Double),

                   -- The scalars
                   f         :: a -> Double,
                   g         :: a -> Double,

                   -- Render to a text value for CSV output
                   render    :: a -> T.Text,

                   -- Header for CSV output
                   header    :: T.Text
               }





{-
    A trivial example follows
-}

trivialExampleWrap :: Double -> Double
trivialExampleWrap x = wrap x (0.0, 1.0)

trivialExampleFromPrior :: Gen RealWorld -> IO (Double, Double)
trivialExampleFromPrior rng = do
    x <- uniform rng
    y <- uniform rng
    return (x, y)


trivialExamplePerturb :: (Double, Double)
                      -> Gen RealWorld
                      -> IO ((Double, Double), Double)
trivialExamplePerturb (x, y) rng = do
    choice <- uniformR (0, 1) rng :: IO Int
    (xNew, yNew) <- case choice of
          0 -> do
                 x' <- trivialExampleWrap <$> (x + ) <$> randh rng
                 return (x', y)
          1 -> do
                 y' <- trivialExampleWrap <$> (y + ) <$> randh rng
                 return (x, y')
          _ -> return (x, y)
    return ((xNew, yNew), 0.0)


trivialExampleF :: (Double, Double) -> Double
trivialExampleF = fst

trivialExampleG :: (Double, Double) -> Double
trivialExampleG = snd

trivialExampleRender :: (Double, Double) -> T.Text
trivialExampleRender (x, y) = T.pack $ show x ++ "," ++ show y

trivialExampleHeader :: T.Text
trivialExampleHeader = T.pack "x,y"

-- The full model
trivialExampleModel :: Model (Double, Double)
trivialExampleModel = Model trivialExampleFromPrior trivialExamplePerturb
                       trivialExampleF trivialExampleG trivialExampleRender
                       trivialExampleHeader

