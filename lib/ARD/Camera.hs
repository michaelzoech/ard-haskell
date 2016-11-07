module ARD.Camera where

import ARD.Vector2 as Vector2
import ARD.Vector3 as Vector3
import ARD.Ray

class Camera a where
  generateRay :: a -> Vector2 -> Ray

data OrthographicCamera
  = OrthographicCamera
  { orthographicEye :: Vector3
  , orthographicLookAt :: Vector3
  , orthographicUp :: Vector3
  , orthographicUVW :: (Vector3, Vector3, Vector3)
  }

instance Camera OrthographicCamera where
  generateRay camera (Vector2 x y) =
    let
      eye = orthographicEye camera
      lookAt = orthographicLookAt camera
      (u,v,w) = orthographicUVW camera
      origin = eye `Vector3.plus` (u `Vector3.multiply` x) `Vector3.plus` (v `Vector3.multiply` y)
      direction = lookAt `Vector3.minus` eye
    in
      Ray origin direction

makeOrthographicCamera :: Vector3 -> Vector3 -> Vector3 -> OrthographicCamera
makeOrthographicCamera eye lookAt up =
  let
    w = Vector3.normalize (eye `Vector3.minus` up)
    u = Vector3.normalize (up `Vector3.cross` w)
    v = w `Vector3.cross` u
  in
    OrthographicCamera
    { orthographicEye = eye
    , orthographicLookAt = lookAt
    , orthographicUp = Vector3.normalize up
    , orthographicUVW = (u,v,w)
    }

