module ARD.Light
  ( mkAmbient
  , mkDirectional
  , mkPoint
  ) where

import qualified ARD.Color as C
import qualified ARD.Ray as Ray
import ARD.Geometric as G
import ARD.Vector as V

import qualified Data.Maybe as Maybe

mkAmbient :: C.Color -> Double -> Light
mkAmbient color ls = Light direction incidenceRadiance inShadow
  where
    direction _ = Vector3 0 0 0
    incidenceRadiance _ = color `C.mul` ls
    inShadow _ _ = False

mkDirectional :: Vector3 -> C.Color -> Double -> Light
mkDirectional invDirection color ls = Light direction incidenceRadiance inShadow
  where
    normalizedInvDirection = normalize invDirection
    direction _ = normalizedInvDirection
    incidenceRadiance _ = color `C.mul` ls
    inShadow ray = any (\f -> Maybe.isJust (f ray))

mkPoint :: Vector3 -> C.Color -> Double -> Light
mkPoint location color ls = Light direction incidenceRadiance inShadow
  where
    direction sr = normalize (location - localHitPoint sr)
    incidenceRadiance _ = color `C.mul` ls
    inShadow ray fs =
      let
        d = V.length (location - Ray.origin ray)
      in
        any (< d) $ Maybe.mapMaybe (\f -> f ray) fs

