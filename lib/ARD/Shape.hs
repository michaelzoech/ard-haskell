module ARD.Shape where
  --( Shape(..)
  --) where

import qualified ARD.Material as Material
import qualified ARD.Math as Math
import qualified ARD.Ray as Ray
import ARD.Vector

import Control.Monad (liftM)
import Prelude hiding (div, length)

data HitResult = HitResult
  { tmin :: !Double
  , shadeRecord :: !ShadeRecord
  }

data ShadeRecord
  = ShadeRecord
  { localHitPoint :: !Point3
  , normal :: !Normal3
  , material :: !Material.Material
  , ray :: !Ray.Ray
  }

data Shape
 -- | Center VU VV VW Material
 = Box !Vector3 !Vector3 !Vector3 !Vector3 !Material.Material
 -- | Center Radius Material
 | Sphere !Vector3 !Double !Material.Material
 -- | Point Normal Material
 | Plane !Point3 !Normal3 !Material.Material

hit :: Shape -> Ray.Ray -> Maybe HitResult
hit box@(Box center vu vv vw material) ray = do
  (tmin,vmin) <- hitPointWithNormal box ray
  let
    o = Ray.origin ray
    d = Ray.direction ray
  return HitResult
    { tmin = tmin
    , shadeRecord = ShadeRecord
        { normal = normalize vmin
        , localHitPoint = o + (d `mul` tmin)
        , material = material
        , ray = ray
        }
    }
hit sphere@(Sphere center radius material) ray = do
  tmin <- shadowHit sphere ray
  let
    o = Ray.origin ray
    d = Ray.direction ray
    s = center
    r = radius
    v = o - s
  return HitResult
    { tmin = tmin
    , shadeRecord = ShadeRecord
        { normal = (v + (d `mul` tmin)) `div` r
        , localHitPoint = o + (d `mul` tmin)
        , material = material
        , ray = ray
        }
    }
hit (Plane point normal material) ray =
  let p = point
      n = normal
      o = Ray.origin ray
      d = Ray.direction ray
      t = (p - o) `dot` n / (d `dot` n)
  in if t > Math.epsilon
       then Just HitResult
         { tmin = t
         , shadeRecord = ShadeRecord
           { localHitPoint = o + (d `mul` t)
           , normal = n
           , material = material
           , ray = ray
           }
         }
       else Nothing

shadowHit :: Shape -> Ray.Ray -> Maybe Double
shadowHit box@Box{} ray = do
  (tmin, _) <- hitPointWithNormal box ray
  return tmin
shadowHit (Sphere center radius _) ray =
  let
    o = Ray.origin ray
    d = Ray.direction ray
    s = center
    r = radius
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
        if t > Math.epsilon then
          Just t
        else if t' > Math.epsilon then
          Just t'
        else
          Nothing
shadowHit (Plane point normal _) ray =
  let p = point
      n = normal
      o = Ray.origin ray
      d = Ray.direction ray
      t = (p - o) `dot` n / (d `dot` n)
  in
    if t > Math.epsilon then Just t else Nothing


hitPointWithNormal :: Shape -> Ray.Ray -> Maybe (Double, Vector3)
hitPointWithNormal (Box center vu vv vw material) ray =
  let
    d = Ray.direction ray
    p = center - Ray.origin ray
    hit' vi ((tmin,vmin),(tmax,vmax)) normal =
      let
        ai = normalize vi
        hi = length vi
        e = ai `dot` p
        f = ai `dot` d
      in
        if abs f > Math.epsilon then
          let
            t1 = (e + hi) / f
            t2 = (e - hi) / f
            t2' = max t2 t1
            t1' = min t2 t1
            tmin' = if t1' > tmin then t1' else tmin
            tmax' = if t2' < tmax then t2' else tmax
            vmin' = if t1' > tmin then normal else vmin
            vmax' = if t2' < tmax then negate normal else vmax
          in
            if tmin' > tmax' || tmax' <= Math.epsilon then
              Nothing
            else
              Just ((tmin',vmin'), (tmax',vmax'))
        else
          Just ((tmin,vmin), (tmax,vmax))
  in do
    u <- hit' vu ((-999999, Vector3 0 0 0), (999999, Vector3 0 0 0)) (vw `cross` vv)
    v <- hit' vv u (vw `cross` vu)
    ((tmin,vmin), (tmax,vmax)) <- hit' vw v (vu `cross` vv)
    return $ if tmin > 0 then
      (tmin,vmin)
    else
      (tmax,vmax)

