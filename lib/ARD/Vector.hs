module ARD.Vector where

import Prelude hiding (length)

class Vector a where
  -- | Multiply Vector by Scalar.
  mul :: a -> Double -> a
  -- | Divide Vector by Scalar.
  div :: a -> Double -> a
  -- | Length of Vector.
  length :: a -> Double
  -- | Squared length of Vector.
  lengthSquared :: a -> Double
  -- | Normalize Vector to unit length.
  normalize :: a -> a
  -- | Scalar (dot) product between two Vectors.
  dot :: a -> a -> Double
  -- | Cross product between two Vectors.
  cross :: a -> a -> a
  -- | Execute function on each Vector component.
  forComponent :: a -> (Double -> Double) -> a

data Vector2 = Vector2 !Double !Double
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

instance Vector Vector2 where
  mul (Vector2 x y) d = Vector2 (x*d) (y*d)
  div (Vector2 x y) d = Vector2 (x/d) (y/d)
  lengthSquared (Vector2 x y) = x*x + y*y
  length v = sqrt $ lengthSquared v
  normalize v = v `mul` (1.0 / length v)
  dot (Vector2 x y) (Vector2 x' y') = x*x' + y*y'
  cross (Vector2 x y) (Vector2 x' y') = Vector2 (y*x' - y*x') (x*y' - y*x')
  forComponent (Vector2 x y) f = Vector2 (f x) (f y)

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

instance Vector Vector3 where
  mul (Vector3 x y z) d = Vector3 (x*d) (y*d) (z*d)
  div (Vector3 x y z) d = Vector3 (x/d) (y/d) (z/d)
  lengthSquared (Vector3 x y z) = x*x + y*y + z*z
  length v = sqrt $ lengthSquared v
  normalize v = v `mul` (1.0 / length v)
  dot (Vector3 x y z) (Vector3 x' y' z') = x*x' + y*y' + z*z'
  cross (Vector3 x y z) (Vector3 x' y' z') = Vector3 (y*z' - z*y') (z*x' - x*z') (x*y' - y*x')
  forComponent (Vector3 x y z) f = Vector3 (f x) (f y) (f z)

