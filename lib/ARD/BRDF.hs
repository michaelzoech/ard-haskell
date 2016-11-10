module ARD.BRDF where

import qualified ARD.Color as C
import ARD.Geometric as G
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

createGlossySpecularBRDF :: C.Color -> Double -> BRDF
createGlossySpecularBRDF ks exp =
  let
    f sr wi wo =
      let
        ndotwi = G.normal sr `dot` wi
        r = (-wi) + G.normal sr `mul` (2 * ndotwi)
        rdotwo = r `dot` wo
      in
        if rdotwo > 0 then
          ks `C.mul` (rdotwo ** exp)
        else
          C.RGB 0 0 0
    rho sr wo = C.RGB 0 0 0
  in
    BRDF f rho
