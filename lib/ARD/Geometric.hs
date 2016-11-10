module ARD.Geometric where

import ARD.Color
import ARD.Ray
import ARD.Vector

type ShadeFunc = ShadeRecord -> [Light] -> Color

data Material
  = Material ShadeFunc

type DirectionFunc = (ShadeRecord -> Vector3)
type IncidenceRadianceFunc = (ShadeRecord -> Color)

data Light
  = Light DirectionFunc IncidenceRadianceFunc

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

