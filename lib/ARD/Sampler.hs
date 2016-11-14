module ARD.Sampler
  ( Sampler(..)
  , mkRegular
  , mkStandard
  ) where

import ARD.Vector

data Sampler
  = Sampler
  { numSamples :: Int
  , unitSquareSamples :: [Point2]
  , hemisphereSamples :: [Vector3]
  } deriving (Eq, Show)

mkRegular :: Int -> Sampler
mkRegular samplesPerAxis =
  let
    n = fromIntegral samplesPerAxis
    points = map (/n) [0.5..n-1]
    samples = [ Vector2 x y | y <- points, x <- points ]
  in
    Sampler (samplesPerAxis*samplesPerAxis) samples (map (unitSquareSampleToHemisphereSample 2) samples)

mkStandard :: Sampler
mkStandard =
  let
    samples = [Vector2 0.5 0.5]
  in
    Sampler 1 samples (map (unitSquareSampleToHemisphereSample 2) samples)

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

