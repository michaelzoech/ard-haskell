module ARD.Camera
  ( Camera(..)
  , mkOrthographic
  , mkPinhole
  ) where

import ARD.Ray
import ARD.Vector

data Camera
  = Camera
  { eye :: Vector3
  , lookAt :: Vector3
  , up :: Vector3
  , uvw :: (Vector3, Vector3, Vector3)
  , generateRay :: Vector2 -> Ray
  }

mkOrthographic :: Vector3 -> Vector3 -> Vector3 -> Camera
mkOrthographic eye lookAt up =
  let
    uvw@(u,v,w) = calculateUVW eye lookAt up
  in
    Camera
    { eye = eye
    , lookAt = lookAt
    , up = normalize up
    , uvw = uvw
    , generateRay = \(Vector2 x y) ->
        let
          origin = eye + (u `mul` x) + (v `mul` y)
          direction = lookAt - eye
        in
          Ray origin direction
    }

mkPinhole :: Vector3 -> Vector3 -> Vector3 -> Double -> Camera
mkPinhole eye lookAt up distance =
  let
    uvw@(u,v,w) = calculateUVW eye lookAt up
  in
    Camera
    { eye = eye
    , lookAt = lookAt
    , up = up
    , uvw = uvw
    , generateRay = \(Vector2 x y) ->
        let
          direction = (u `mul` x) + (v `mul` y) - (w `mul` distance)
        in
          Ray eye (normalize direction)
    }

calculateUVW :: Vector3 -> Vector3 -> Vector3 -> (Vector3, Vector3, Vector3)
calculateUVW eye lookAt up =
  let
    w = normalize (eye - lookAt)
    u = normalize (up `cross` w)
    v = w `cross` u
  in
    (u, v, w)

