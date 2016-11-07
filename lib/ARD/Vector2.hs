module ARD.Vector2 where

import Prelude hiding (length)

data Vector2 = Vector2 Double Double
  deriving (Eq, Show)

type Normal2 = Vector2

type Point2 = Vector2

instance Num Vector2 where
  (+) (Vector2 x y) (Vector2 x' y') = Vector2 (x+x') (y+y')
  (*) (Vector2 x y) (Vector2 x' y') = Vector2 (x*x') (y*y')
  (-) (Vector2 x y) (Vector2 x' y') = Vector2 (x-x') (y-y')
  negate v = v `forComponent` negate
  abs v = v `forComponent` abs
  signum v = v `forComponent` signum
  fromInteger i =
    let
      i' = fromIntegral i
    in
      Vector2 i' i'

forComponent :: Vector2 -> (Double -> Double) -> Vector2
forComponent (Vector2 x y) f = Vector2 (f x) (f y)

-- | Returns squared length of given Vector2.
lengthSquared :: Vector2 -> Double
lengthSquared (Vector2 x y) = x*x + y*y

-- | Returns length of given Vector2.
length :: Vector2 -> Double
length v = sqrt $ lengthSquared v

multiply :: Vector2 -> Double -> Vector2
multiply (Vector2 x y) d = Vector2 (x*d) (y*d)

normalize :: Vector2 -> Vector2
normalize v = v `multiply` (1.0 / length v)

