module ARD.Light where

import qualified ARD.Color as C
import ARD.Geometric
import ARD.Vector

createAmbient :: C.Color -> Double -> Light
createAmbient color ls =
  let
    direction sr = Vector3 0 0 0
    incidenceRadiance sr = color `C.mul` ls
  in
    Light direction incidenceRadiance

createPoint :: Vector3 -> C.Color -> Double -> Light
createPoint location color ls =
  let
    direction sr = normalize (location - localHitPoint sr)
    incidenceRadiance sr = color `C.mul` ls
  in
    Light direction incidenceRadiance

createDirectional :: Vector3 -> C.Color -> Double -> Light
createDirectional invDirection color ls =
  let
    direction sr = invDirection
    incidenceRadiance sr = color `C.mul` ls
  in
    Light direction incidenceRadiance

