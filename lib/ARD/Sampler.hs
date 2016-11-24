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
  { numSamples :: !Int
  , numSets :: !Int
  , unitSquareSamples :: ![[Point2]]
  , hemisphereSamples :: ![[Vector3]]
  } deriving (Eq, Show)

mkJittered :: Int -> Randomize Sampler
mkJittered samplesPerAxis = mkSampler (samplesPerAxis*samplesPerAxis) defaultNumSets $ do
  randoms <- getRandoms (samplesPerAxis*samplesPerAxis*2)
  let
    n = fromIntegral samplesPerAxis
    invN = 1 / n
    points = [0..n-1]
    unscaledSamples = [ (x,y) | y <- points, x <- points ]
    randomPairs = mapAdjacent (,) randoms
    samples = map (\((x,y),(a,b)) -> Vector2 ((x+a)*invN) ((y+b)*invN) ) $ zip unscaledSamples randomPairs
  shuffle samples

mkRandom :: Int -> Randomize Sampler
mkRandom nSamples = mkSampler nSamples defaultNumSets $ do
  randoms <- getRandoms (nSamples*2)
  return $ take nSamples $ mapAdjacent Vector2 randoms

mkRegular :: Int -> Randomize Sampler
mkRegular samplesPerAxis = mkSampler (samplesPerAxis*samplesPerAxis) defaultNumSets $ do
  let
    n = fromIntegral samplesPerAxis
    invN = 1 / n
    points = map (*invN) [0.5..n-1]
    samples = [ Vector2 x y | y <- points, x <- points ]
  shuffle samples

mkStandard :: Randomize Sampler
mkStandard = mkSampler 1 1 (return [Vector2 0.5 0.5])

mkSampler :: Int -> Int -> Randomize [Point2] -> Randomize Sampler
mkSampler nSamples nSets sampleGen = do
  sets <- replicateM nSets sampleGen
  return Sampler
    { numSamples = nSamples
    , numSets = nSets
    , unitSquareSamples = sets
    , hemisphereSamples = (map . map) (unitSquareSampleToHemisphereSample 2) sets
    }

defaultNumSets :: Int
defaultNumSets = 83

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

