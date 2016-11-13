module ARD.Geometric where

import ARD.Material
import ARD.Ray
import ARD.Vector

data HitResult = HitResult
  { tmin :: Double
  , shadeRecord :: ShadeRecord
  }

data ShadeRecord
  = ShadeRecord
  { localHitPoint :: Point3
  , normal :: Normal3
  , material :: Material
  , ray :: Ray
  }

class GeometricObject a where
  hit :: a -> Ray -> Maybe HitResult
  shadowHit :: a -> Ray -> Maybe Double

