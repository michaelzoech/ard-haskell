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
  } deriving (Eq, Show)

mkRegular :: Int -> Sampler
mkRegular samplesPerAxis =
  let
    n = fromIntegral samplesPerAxis
    points = map (/n) [0.5..n-1]
    samples = [ Vector2 x y | y <- points, x <- points ]
  in
    Sampler (samplesPerAxis*samplesPerAxis) samples

mkStandard :: Sampler
mkStandard = Sampler 1 [Vector2 0.5  0.5]

