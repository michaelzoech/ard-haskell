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
      origin = eye + (u `Vector3.mul` x) + (v `Vector3.mul` y)
      direction = lookAt - eye
    in
      Ray origin direction

makeOrthographicCamera :: Vector3 -> Vector3 -> Vector3 -> OrthographicCamera
makeOrthographicCamera eye lookAt up =
  OrthographicCamera
  { orthographicEye = eye
  , orthographicLookAt = lookAt
  , orthographicUp = Vector3.normalize up
  , orthographicUVW = calculateUVW eye lookAt up
  }

data PinholeCamera
  = PinholeCamera
  { pinholeEye :: Vector3
  , pinholeLookAt :: Vector3
  , pinholeUp :: Vector3
  , pinholeUVW :: (Vector3, Vector3, Vector3)
  , pinholeViewPlaneDistance :: Double
  }

instance Camera PinholeCamera where
  generateRay camera (Vector2 x y) =
    let
      eye = pinholeEye camera
      distance = pinholeViewPlaneDistance camera
      (u,v,w) = pinholeUVW camera
      direction = (u `Vector3.mul` x) + (v `Vector3.mul` y) - (w `Vector3.mul` distance)
    in
      Ray eye (Vector3.normalize direction)

makePinholeCamera :: Vector3 -> Vector3 -> Vector3 -> Double -> PinholeCamera
makePinholeCamera eye lookAt up d =
  PinholeCamera
  { pinholeEye = eye
  , pinholeLookAt = lookAt
  , pinholeUp = up
  , pinholeUVW = calculateUVW eye lookAt up
  , pinholeViewPlaneDistance = d
  }

calculateUVW :: Vector3 -> Vector3 -> Vector3 -> (Vector3, Vector3, Vector3)
calculateUVW eye lookAt up =
  let
    w = Vector3.normalize (eye - lookAt)
    u = Vector3.normalize (up `Vector3.cross` w)
    v = w `Vector3.cross` u
  in
    (u, v, w)

