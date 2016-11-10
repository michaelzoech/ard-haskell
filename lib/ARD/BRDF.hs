module ARD.BRDF where

import qualified ARD.Color as C
import ARD.Geometric
import ARD.Vector

data BRDF
  = BRDF (ShadeRecord -> Vector3 -> Vector3 -> C.Color) (ShadeRecord -> Vector3 -> C.Color)

createLambertianBRDF :: C.Color -> Double -> BRDF
createLambertianBRDF cd kd =
  let
    f sr wi wo = cd `C.mul` (kd * (1.0 / pi))
    rho sr wo = cd `C.mul` kd
    --sampleF = 
  in
    BRDF f rho

