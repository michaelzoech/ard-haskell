module ARD.Plane where

import qualified ARD.Geometric as G
import ARD.Material
import qualified ARD.Math as Math
import ARD.Ray as Ray
import ARD.Vector

data Plane
  = Plane
  { point :: Point3
  , normal :: Normal3
  , material :: Material
  }

instance G.GeometricObject Plane where
  hit plane ray =
    let p = point plane
        n = normal plane
        o = Ray.origin ray
        d = Ray.direction ray
        t = (p - o) `dot` n / (d `dot` n)
    in if t > Math.epsilon
         then Just G.HitResult
           { G.tmin = t
           , G.shadeRecord = G.ShadeRecord
             { G.localHitPoint = o + (d `mul` t)
             , G.normal = n
             , G.material = material plane
             , G.ray = ray
             }
           }
         else Nothing
  shadowHit plane ray =
    let p = point plane
        n = normal plane
        o = Ray.origin ray
        d = Ray.direction ray
        t = (p - o) `dot` n / (d `dot` n)
    in
      if t > Math.epsilon then Just t else Nothing

