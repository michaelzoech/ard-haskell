module ARD.Vector3 where

import Prelude hiding (length)

data Vector3 = Vector3 Double Double Double
  deriving (Eq, Show)

type Normal3 = Vector3

type Point3 = Vector3

-- | Returns squared length of given Vector3.
lengthSquared :: Vector3 -> Double
lengthSquared (Vector3 x y z) = x*x + y*y + z*z

-- | Returns length of given Vector3.
length :: Vector3 -> Double
length v = sqrt $ lengthSquared v

plus :: Vector3 -> Vector3 -> Vector3
plus (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z')

minus :: Vector3 -> Vector3 -> Vector3
minus (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x-x') (y-y') (z-z')

multiply :: Vector3 -> Double -> Vector3
multiply (Vector3 x y z) d = Vector3 (x*d) (y*d) (z*d)

dot :: Vector3 -> Vector3 -> Double
dot (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'

normalize :: Vector3 -> Vector3
normalize v = v `multiply` (1.0 / length v)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x y z) (Vector3 x' y' z') = Vector3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')

