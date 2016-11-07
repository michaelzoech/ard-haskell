module ARD.Vector2 where

import Prelude hiding (length)

data Vector2 = Vector2 Double Double
  deriving (Eq, Show)

type Normal2 = Vector2

type Point2 = Vector2

-- | Returns squared length of given Vector2.
lengthSquared :: Vector2 -> Double
lengthSquared (Vector2 x y) = x*x + y*y

-- | Returns length of given Vector2.
length :: Vector2 -> Double
length v = sqrt $ lengthSquared v

plus :: Vector2 -> Vector2 -> Vector2
plus (Vector2 x y) (Vector2 x' y') = Vector2 (x+x') (y+y')

minus :: Vector2 -> Vector2 -> Vector2
minus (Vector2 x y) (Vector2 x' y') = Vector2 (x-x') (y-y')

multiply :: Vector2 -> Double -> Vector2
multiply (Vector2 x y) d = Vector2 (x*d) (y*d)

normalize :: Vector2 -> Vector2
normalize v = v `multiply` (1.0 / length v)

