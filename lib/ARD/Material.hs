module ARD.Material where

import qualified ARD.Color as C
import ARD.BRDF
import ARD.Geometric as G
import ARD.Light as Light
import qualified ARD.Ray as Ray
import ARD.Vector

createMatte :: C.Color -> Double -> Double -> Material
createMatte cd kd ka =
  let
    BRDF ambientL ambientRho = createLambertianBRDF cd ka
    BRDF diffuseL diffuseRho = createLambertianBRDF cd kd
    func sr lights =
      let
        wo = -(Ray.direction $ G.ray sr)
        ambientRadiance = ambientRho sr wo
        lightFunc sr (Light directionFunc shadeFunc) =
          let
            wi = directionFunc sr
            ndotwi = G.normal sr `dot` wi
          in
            if ndotwi > 0 then
              diffuseL sr wi wo * shadeFunc sr `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc sr) ambientRadiance lights
  in
    Material func

createPhong :: C.Color -> Double -> Double -> C.Color -> Double -> Material
createPhong cd kd ka ks exp =
  let
    BRDF ambientL ambientRho = createLambertianBRDF cd ka
    BRDF diffuseL diffuseRho = createLambertianBRDF cd kd
    BRDF specularL specularRho = createGlossySpecularBRDF ks exp
    func sr lights =
      let
        wo = -(Ray.direction $ G.ray sr)
        ambientRadiance = ambientRho sr wo
        lightFunc sr (Light directionFunc shadeFunc) =
          let
            wi = directionFunc sr
            ndotwi = G.normal sr `dot` wi
          in
            if ndotwi > 0 then
              (diffuseL sr wi wo + specularL sr wi wo) * shadeFunc sr `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc sr) ambientRadiance lights
  in
    Material func
  
