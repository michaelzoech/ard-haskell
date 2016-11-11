module ARD.Light where

import qualified ARD.Color as C
import ARD.Geometric
import ARD.Vector

mkAmbient :: C.Color -> Double -> Light
mkAmbient color ls = Light direction incidenceRadiance
  where
    direction _ = Vector3 0 0 0
    incidenceRadiance _ = color `C.mul` ls

mkPoint :: Vector3 -> C.Color -> Double -> Light
mkPoint location color ls = Light direction incidenceRadiance
  where
    direction sr = normalize (location - localHitPoint sr)
    incidenceRadiance _ = color `C.mul` ls

mkDirectional :: Vector3 -> C.Color -> Double -> Light
mkDirectional invDirection color ls = Light direction incidenceRadiance
  where
    direction _ = invDirection
    incidenceRadiance _ = color `C.mul` ls

