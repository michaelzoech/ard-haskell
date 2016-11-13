module ARD.Sphere where

import qualified ARD.Geometric as G
import ARD.Material
import qualified ARD.Ray as Ray
import ARD.Vector

import Prelude hiding (div)

data Sphere
  = Sphere
  { center :: Point3
  , radius :: Double
  , material :: Material
  }

instance G.GeometricObject Sphere where
  hit sphere ray = do
    tmin <- G.shadowHit sphere ray
    let
      o = Ray.origin ray
      d = Ray.direction ray
      s = center sphere
      r = radius sphere
      v = o - s
    return G.HitResult
      { G.tmin = tmin
      , G.shadeRecord = G.ShadeRecord
          { G.normal = (v + (d `mul` tmin)) `div` r
          , G.localHitPoint = o + (d `mul` tmin)
          , G.material = material sphere
          , G.ray = ray
          }
      }
  shadowHit sphere ray =
    let
      o = Ray.origin ray
      d = Ray.direction ray
      s = center sphere
      r = radius sphere
      v = o - s
      a = d `dot` d
      b = v `mul` 2 `dot` d
      c = v `dot` v - r * r
      disc = b * b - 4 * a * c
    in
      if disc < 0.0
      then
        Nothing
      else
        let
          e = sqrt disc
          denom = 2 * a
          t = (-b - e) / denom
          t' = (-b + e) / denom
        in
          if t > 1.0e-8 then
            Just t
          else if t' > 1.0e-8 then
            Just t'
          else
            Nothing

