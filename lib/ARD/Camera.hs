module ARD.Camera where

import ARD.Vector as Vector
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
      origin = eye + (u `Vector.mul` x) + (v `Vector.mul` y)
      direction = lookAt - eye
    in
      Ray origin direction

makeOrthographicCamera :: Vector3 -> Vector3 -> Vector3 -> OrthographicCamera
makeOrthographicCamera eye lookAt up =
  OrthographicCamera
  { orthographicEye = eye
  , orthographicLookAt = lookAt
  , orthographicUp = Vector.normalize up
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
      direction = (u `Vector.mul` x) + (v `Vector.mul` y) - (w `Vector.mul` distance)
    in
      Ray eye (Vector.normalize direction)

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
    w = Vector.normalize (eye - lookAt)
    u = Vector.normalize (up `Vector.cross` w)
    v = w `Vector.cross` u
  in
    (u, v, w)

