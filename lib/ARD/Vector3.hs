module ARD.Vector3 where

import Prelude hiding (length)

data Vector3 = Vector3 !Double !Double !Double
  deriving (Eq, Show)

type Normal3 = Vector3

type Point3 = Vector3

instance Num Vector3 where
  (+) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x+x') (y+y') (z+z')
  (*) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x*x') (y*y') (z*z')
  (-) (Vector3 x y z) (Vector3 x' y' z') = Vector3 (x-x') (y-y') (z-z')
  negate v = v `forComponent` negate
  abs v = v `forComponent` abs
  signum v = v `forComponent` signum
  fromInteger i =
    let
      i' = fromIntegral i
    in
      Vector3 i' i' i'

forComponent :: Vector3 -> (Double -> Double) -> Vector3
forComponent (Vector3 x y z) f = Vector3 (f x) (f y) (f z)

-- | Returns squared length of given Vector3.
lengthSquared :: Vector3 -> Double
lengthSquared (Vector3 x y z) = x*x + y*y + z*z

-- | Returns length of given Vector3.
length :: Vector3 -> Double
length v = sqrt $ lengthSquared v

mul :: Vector3 -> Double -> Vector3
mul (Vector3 x y z) d = Vector3 (x*d) (y*d) (z*d)

div :: Vector3 -> Double -> Vector3
div (Vector3 x y z) d = Vector3 (x/d) (y/d) (z/d)

dot :: Vector3 -> Vector3 -> Double
dot (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'

normalize :: Vector3 -> Vector3
normalize v = v `mul` (1.0 / length v)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vector3 x y z) (Vector3 x' y' z') = Vector3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')

