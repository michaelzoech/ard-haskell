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
mkRegular numSamples =
  let
    n = (truncate $ sqrt $ fromIntegral numSamples) :: Int
    n' = fromIntegral n
    samples = [ Vector2 ((fromIntegral k + 0.5) / n') ((fromIntegral j + 0.5) / n') | j <- [0..n-1], k <- [0..n-1] ]
  in
    Sampler (Prelude.length samples) samples

mkStandard :: Sampler
mkStandard = Sampler 1 [Vector2 0.5  0.5]

