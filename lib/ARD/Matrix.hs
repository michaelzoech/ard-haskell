module ARD.Matrix where

import ARD.Vector
import qualified ARD.Math as Math

import qualified Data.Vector.Unboxed as UV

newtype Matrix = Matrix { mat :: UV.Vector Double }

instance Eq Matrix where
  (==) a b = mat a == mat b

instance Show Matrix where
  show = show . mat

translate :: Vector3 -> Matrix
translate (Vector3 x y z) =
  matrix
    1 0 0 x
    0 1 0 y
    0 0 1 z
    0 0 0 1

rotateX :: Double -> Matrix
rotateX deg =
  let
    sinr = sin (Math.radians deg)
    cosr = cos (Math.radians deg)
  in
    matrix
      1    0       0 0
      0 cosr (-sinr) 0
      0 sinr    cosr 0
      0    0       0 1

rotateY :: Double -> Matrix
rotateY deg =
  let
    sinr = sin (Math.radians deg)
    cosr = cos (Math.radians deg)
  in
    matrix
         cosr 0 sinr 0
            0 1    0 0
      (-sinr) 0 cosr 0
            0 0    0 1

rotateZ :: Double -> Matrix
rotateZ deg =
  let
    sinr = sin (Math.radians deg)
    cosr = cos (Math.radians deg)
  in
    matrix
      cosr (-sinr) 0 0
      sinr    cosr 0 0
      0          0 1 0
      0          0 0 1

identity :: Matrix
identity =
  matrix
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1

matrix :: Double -> Double -> Double -> Double ->
          Double -> Double -> Double -> Double ->
          Double -> Double -> Double -> Double ->
          Double -> Double -> Double -> Double -> Matrix
matrix a b c d e f g h i j k l m n o p =
  Matrix $ UV.fromList [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p]

mi :: Matrix -> Int -> Int -> Double
{-# INLINE mi #-}
mi m r c = UV.unsafeIndex (mat m) (r*4 + c)

mul :: Matrix -> Matrix -> Matrix
mul a b =
  matrix
    (mi a 0 0 * mi b 0 0 + mi a 0 1 * mi b 1 0 + mi a 0 2 * mi b 2 0 + mi a 0 3 * mi b 3 0)
    (mi a 0 0 * mi b 0 1 + mi a 0 1 * mi b 1 1 + mi a 0 2 * mi b 2 1 + mi a 0 3 * mi b 3 1)
    (mi a 0 0 * mi b 0 2 + mi a 0 1 * mi b 1 2 + mi a 0 2 * mi b 2 2 + mi a 0 3 * mi b 3 2)
    (mi a 0 0 * mi b 0 3 + mi a 0 1 * mi b 1 3 + mi a 0 2 * mi b 2 3 + mi a 0 3 * mi b 3 3)

    (mi a 1 0 * mi b 0 0 + mi a 1 1 * mi b 1 0 + mi a 1 2 * mi b 2 0 + mi a 1 3 * mi b 3 0)
    (mi a 1 0 * mi b 0 1 + mi a 1 1 * mi b 1 1 + mi a 1 2 * mi b 2 1 + mi a 1 3 * mi b 3 1)
    (mi a 1 0 * mi b 0 2 + mi a 1 1 * mi b 1 2 + mi a 1 2 * mi b 2 2 + mi a 1 3 * mi b 3 2)
    (mi a 1 0 * mi b 0 3 + mi a 1 1 * mi b 1 3 + mi a 1 2 * mi b 2 3 + mi a 1 3 * mi b 3 3)

    (mi a 2 0 * mi b 0 0 + mi a 2 1 * mi b 1 0 + mi a 2 2 * mi b 2 0 + mi a 2 3 * mi b 3 0)
    (mi a 2 0 * mi b 0 1 + mi a 2 1 * mi b 1 1 + mi a 2 2 * mi b 2 1 + mi a 2 3 * mi b 3 1)
    (mi a 2 0 * mi b 0 2 + mi a 2 1 * mi b 1 2 + mi a 2 2 * mi b 2 2 + mi a 2 3 * mi b 3 2)
    (mi a 2 0 * mi b 0 3 + mi a 2 1 * mi b 1 3 + mi a 2 2 * mi b 2 3 + mi a 2 3 * mi b 3 3)

    (mi a 3 0 * mi b 0 0 + mi a 3 1 * mi b 1 0 + mi a 3 2 * mi b 2 0 + mi a 3 3 * mi b 3 0)
    (mi a 3 0 * mi b 0 1 + mi a 3 1 * mi b 1 1 + mi a 3 2 * mi b 2 1 + mi a 3 3 * mi b 3 1)
    (mi a 3 0 * mi b 0 2 + mi a 3 1 * mi b 1 2 + mi a 3 2 * mi b 2 2 + mi a 3 3 * mi b 3 2)
    (mi a 3 0 * mi b 0 3 + mi a 3 1 * mi b 1 3 + mi a 3 2 * mi b 2 3 + mi a 3 3 * mi b 3 3)

transformVector :: Matrix -> Vector3 -> Vector3
transformVector m (Vector3 x y z) = Vector3 x' y' z'
  where
    x' = mi m 0 0 * x + mi m 0 1 * y + mi m 0 2 * z
    y' = mi m 1 0 * x + mi m 1 1 * y + mi m 1 2 * z
    z' = mi m 2 0 * x + mi m 2 1 * y + mi m 2 2 * z

transformPoint :: Matrix -> Point3 -> Point3
transformPoint m (Vector3 x y z) = Vector3 x' y' z'
  where
    vx = mi m 0 0 * x + mi m 0 1 * y + mi m 0 2 * z + mi m 0 3
    vy = mi m 1 0 * x + mi m 1 1 * y + mi m 1 2 * z + mi m 1 3
    vz = mi m 2 0 * x + mi m 2 1 * y + mi m 2 2 * z + mi m 2 3
    vw = mi m 3 0 * x + mi m 3 1 * y + mi m 3 2 * z + mi m 3 3
    oneByW = 1.0 / vw
    (x', y', z') = (vx/oneByW, vy/oneByW, vz/oneByW)

