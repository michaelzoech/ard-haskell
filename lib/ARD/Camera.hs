module ARD.Camera
  ( Camera
  , mkOrthographic
  , mkPinhole
  , generateRay
  , uvw
  ) where

import ARD.Ray
import ARD.Vector

data Camera
  = Orthographic Vector3 Vector3 Vector3 (Vector3, Vector3, Vector3)
  | Pinhole Vector3 Double (Vector3, Vector3, Vector3)

mkOrthographic :: Vector3 -> Vector3 -> Vector3 -> Camera
mkOrthographic eye lookAt up =
  let
    uvw@(u,v,w) = calculateUVW eye lookAt up
  in
    Orthographic eye lookAt (lookAt-eye) uvw

mkPinhole :: Vector3 -> Vector3 -> Vector3 -> Double -> Camera
mkPinhole eye lookAt up distance =
  let
    uvw@(u,v,w) = calculateUVW eye lookAt up
  in
    Pinhole eye distance uvw

generateRay :: Camera -> Vector2 -> Ray
generateRay (Orthographic eye lookAt direction (u,v,_)) (Vector2 x y) =
  let
    origin = eye + (u `mul` x) + (v `mul` y)
    direction = lookAt - eye
  in
    Ray origin direction
generateRay (Pinhole eye distance (u,v,w)) (Vector2 x y) =
  let
    direction = (u `mul` x) + (v `mul` y) - (w `mul` distance)
  in
    Ray eye (normalize direction)

uvw :: Camera -> (Vector3, Vector3, Vector3)
uvw (Orthographic _ _ _ uvw) = uvw
uvw (Pinhole _ _ uvw) = uvw

calculateUVW :: Vector3 -> Vector3 -> Vector3 -> (Vector3, Vector3, Vector3)
calculateUVW eye lookAt up =
  let
    w = normalize (eye - lookAt)
    u = normalize (up `cross` w)
    v = w `cross` u
  in
    (u, v, w)

