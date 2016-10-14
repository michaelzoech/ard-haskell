module ARD.Geometric where

import ARD.Ray
import ARD.Vector3

data HitResult = HitResult
  { tmin :: Double
  , shadeRecord :: ShadeRecord
  }

data RGBColor = RGBColor Double Double Double

data ShadeRecord
  = ShadeRecord
  { localHitPoint :: Point3
  , normal :: Normal3
  , color :: RGBColor
  }

class GeometricObject a where
  hit :: a -> Ray -> Maybe HitResult

