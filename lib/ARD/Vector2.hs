module ARD.Vector2 where

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

