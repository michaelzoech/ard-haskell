module ARD.Box
  ( Box(..)
  , G.GeometricObject(..)
  ) where

import qualified ARD.Geometric as G
import qualified ARD.Material as Material
import qualified ARD.Math as Math
import qualified ARD.Ray as Ray
import ARD.Vector

import Control.Monad (liftM)
import Prelude hiding (length)

data Box
  = Box
  { center :: Vector3
  , vu :: Vector3
  , vv :: Vector3
  , vw :: Vector3
  , material :: Material.Material
  }

instance G.GeometricObject Box where
  hit box ray = do
    (tmin,vmin) <- hitPointWithNormal box ray
    let
      o = Ray.origin ray
      d = Ray.direction ray
    return G.HitResult
      { G.tmin = tmin
      , G.shadeRecord = G.ShadeRecord
          { G.normal = normalize vmin
          , G.localHitPoint = o + (d `mul` tmin)
          , G.material = material box
          , G.ray = ray
          }
      }
  shadowHit box ray = do
    (tmin, _) <- hitPointWithNormal box ray
    return tmin

hitPointWithNormal box ray =
  let
    d = Ray.direction ray
    p = center box - Ray.origin ray
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
    u <- hit' (vu box) ((-999999, Vector3 0 0 0), (999999, Vector3 0 0 0)) (vw box `cross` vv box)
    v <- hit' (vv box) u (vw box `cross` vu box)
    ((tmin,vmin), (tmax,vmax)) <- hit' (vw box) v (vu box `cross` vv box)
    return $ if tmin > 0 then
      (tmin,vmin)
    else
      (tmax,vmax)

