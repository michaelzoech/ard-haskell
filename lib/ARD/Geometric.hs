module ARD.Geometric where

import ARD.Color
import ARD.Ray
import ARD.Vector

data HitResult = HitResult
  { tmin :: Double
  , shadeRecord :: ShadeRecord
  }
  deriving (Eq, Show)

data ShadeRecord
  = ShadeRecord
  { localHitPoint :: Point3
  , normal :: Normal3
  , color :: Color
  }
  deriving (Eq, Show)

class GeometricObject a where
  hit :: a -> Ray -> Maybe HitResult

