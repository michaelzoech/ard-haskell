module ARD.Sphere where

import qualified ARD.Geometric as G
import qualified ARD.Ray as Ray
import ARD.Vector3

data Sphere
  = Sphere
  { center :: Point3
  , radius :: Double
  }

instance G.GeometricObject Sphere where
  hit sphere ray =
    let
      o = Ray.origin ray
      d = Ray.direction ray
      s = center sphere
      r = radius sphere
      v = o `minus` s
      a = d `dot` d
      b = v `multiply` 2 `dot` d
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
            Just $ G.HitResult
              { G.tmin = t
              , G.shadeRecord = G.ShadeRecord
                  { G.normal = (v `plus` (d `multiply` t)) `divide` r
                  , G.localHitPoint = o `plus` (d `multiply` t)
                  , G.color = G.RGBColor 1.0 1.0 1.0
                  }
              }
        in
          if t > 1.0e-8 then
            hitResult t
          else if t' > 1.0e-8 then
            hitResult t'
          else
            Nothing

