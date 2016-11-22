module ARD.Sampler
  ( Sampler(..)
  , mkJittered
  , mkRandom
  , mkRegular
  , mkStandard
  ) where

import ARD.Randomize
import ARD.Vector
import Control.Monad
import Control.Monad.State

data Sampler
  = Sampler
  { numSamples :: Int
  , unitSquareSamples :: [Point2]
  , hemisphereSamples :: [Vector3]
  } deriving (Eq, Show)

mkJittered :: Int -> Randomize Sampler
mkJittered samplesPerAxis = do
  randoms <- getRandoms (samplesPerAxis*samplesPerAxis*2)
  let
    n = fromIntegral samplesPerAxis
    invN = 1 / n
    points = [0..n-1]
    unscaledSamples = [ (x,y) | y <- points, x <- points ]
    randomPairs = mapAdjacent (,) randoms
    samples = map (\((x,y),(a,b)) -> Vector2 ((x+a)*invN) ((y+b)*invN) ) $ zip unscaledSamples randomPairs
  return Sampler
      { numSamples = samplesPerAxis * samplesPerAxis
      , unitSquareSamples = samples
      , hemisphereSamples = map (unitSquareSampleToHemisphereSample 2) samples
      }

mkRandom :: Int -> Randomize Sampler
mkRandom numSamples = do
  randoms <- getRandoms (numSamples*2)
  let
    samples = take numSamples $ mapAdjacent Vector2 randoms
  return Sampler
      { numSamples = numSamples
      , unitSquareSamples = samples
      , hemisphereSamples = map (unitSquareSampleToHemisphereSample 2) samples
      }

mkRegular :: Int -> Sampler
mkRegular samplesPerAxis =
  let
    n = fromIntegral samplesPerAxis
    invN = 1 / n
    points = map (*invN) [0.5..n-1]
    samples = [ Vector2 x y | y <- points, x <- points ]
  in
    Sampler (samplesPerAxis*samplesPerAxis) samples (map (unitSquareSampleToHemisphereSample 2) samples)

mkStandard :: Sampler
mkStandard =
  let
    samples = [Vector2 0.5 0.5]
  in
    Sampler 1 samples (map (unitSquareSampleToHemisphereSample 2) samples)

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f (x:y:zs) = f x y : mapAdjacent f zs
mapAdjacent _ _ = []

unitSquareSampleToHemisphereSample :: Double -> Point2 -> Vector3
unitSquareSampleToHemisphereSample e (Vector2 x y) =
  let
    cosphi = cos (2 * pi * x)
    sinphi = sin (2 * pi * x)
    costheta = (1 - y) ** (1 / (e+1))
    sintheta = sqrt (1 - costheta * costheta)
    pu = sintheta * cosphi
    pv = sintheta * sinphi
    pw = costheta
  in
    Vector3 pu pv pw

