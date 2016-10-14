module ARD.Geometric where

import ARD.Ray
import ARD.Vector3

data HitResult = HitResult
  { tmin :: Double
  , shadeRecord :: ShadeRecord
  }
  deriving (Eq, Show)

data RGBColor = RGBColor Double Double Double
  deriving (Eq, Show)

data ShadeRecord
  = ShadeRecord
  { localHitPoint :: Point3
  , normal :: Normal3
  , color :: RGBColor
  }
  deriving (Eq, Show)

class GeometricObject a where
  hit :: a -> Ray -> Maybe HitResult

