module ARD.Sphere where

import Prelude hiding (div)
import qualified ARD.Geometric as G
import qualified ARD.Ray as Ray
import ARD.Vector

data Sphere
  = Sphere
  { center :: Point3
  , radius :: Double
  , material :: G.Material
  }

instance G.GeometricObject Sphere where
  hit sphere ray =
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
          hitResult t =
            Just G.HitResult
              { G.tmin = t
              , G.shadeRecord = G.ShadeRecord
                  { G.normal = (v + (d `mul` t)) `div` r
                  , G.localHitPoint = o + (d `mul` t)
                  , G.material = material sphere
                  , G.ray = ray
                  }
              }
        in
          if t > 1.0e-8 then
            hitResult t
          else if t' > 1.0e-8 then
            hitResult t'
          else
            Nothing

