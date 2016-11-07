module ARD.Plane where

import ARD.Color
import qualified ARD.Geometric as G
import ARD.Ray as Ray
import ARD.Vector3

data Plane
  = Plane
  { point :: Point3
  , normal :: Normal3
  , color :: Color
  }

kEpsilon = 1.0e-8

instance G.GeometricObject Plane where
  hit plane ray =
    let p = point plane
        n = normal plane
        o = Ray.origin ray
        d = Ray.direction ray
        t = (p - o) `dot` n / (d `dot` n)
    in if t > kEpsilon
         then Just G.HitResult
           { G.tmin = t
           , G.shadeRecord = G.ShadeRecord
             { G.localHitPoint = o + (d `multiply` t)
             , G.normal = n
             , G.color = color plane
             }
           }
         else Nothing

