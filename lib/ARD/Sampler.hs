module ARD.Sampler where

import ARD.Vector

data Sampler
  = Sampler
  { numSamples :: Int
  , unitSquareSamples :: [Point2]
  } deriving (Eq, Show)

genStandardSampler :: Sampler
genStandardSampler = Sampler 1 [Vector2 0.5  0.5]

genRegularSampler :: Int -> Sampler
genRegularSampler numSamples =
  let
    n = (truncate $ sqrt $ fromIntegral numSamples) :: Int
    n' = fromIntegral n
    samples = [ Vector2 ((fromIntegral k + 0.5) / n') ((fromIntegral j + 0.5) / n') | j <- [0..n-1], k <- [0..n-1] ]
  in
    Sampler (Prelude.length samples) samples

