module ARD.Vector3 where

data Vector3 = Vector3 Double Double Double
  deriving (Eq, Show)

-- | Returns squared length of given Vector3.
lengthSquared :: Vector3 -> Double
lengthSquared (Vector3 x y z) = x*x + y*y + z*z

-- | Returns length of given Vector3.
length :: Vector3 -> Double
length v = sqrt $ lengthSquared v

