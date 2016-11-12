module ARD.Geometric where

import ARD.Color
import ARD.Ray
import ARD.Vector

type ShadeFunc = ShadeRecord -> [Light] -> [Ray -> Maybe Double] -> Color

data Material
  = Material ShadeFunc

type DirectionFunc = (ShadeRecord -> Vector3)
type IncidenceRadianceFunc = (ShadeRecord -> Color)
type InShadowFunc = (Ray -> [Ray -> Maybe Double] -> Bool)

data Light
  = Light DirectionFunc IncidenceRadianceFunc InShadowFunc

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

