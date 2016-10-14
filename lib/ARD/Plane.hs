module ARD.Plane where

import qualified ARD.Geometric as G
import ARD.Ray as Ray
import ARD.Vector3

data Plane
  = Plane
  { point :: Point3
  , normal :: Normal3
  }

kEpsilon = 1.0e-8

instance G.GeometricObject Plane where
  hit plane ray =
    let p = point plane
        n = normal plane
        o = Ray.origin ray
        d = Ray.direction ray
        t = (p `minus` o) `dot` n / (d `dot` n)
    in if t > kEpsilon
         then Just G.HitResult
           { G.tmin = t
           , G.shadeRecord = G.ShadeRecord
             { G.localHitPoint = o `plus` (d `multiply` t)
             , G.normal = n
             , G.color = G.RGBColor 1.0 1.0 1.0
             }
           }
         else Nothing

