module ARD.Material
  ( mkMatte
  , mkPhong
  ) where

import qualified ARD.Color as C
import qualified ARD.BRDF as BRDF
import ARD.Geometric as G
import ARD.Light as Light
import qualified ARD.Ray as Ray
import ARD.Vector

mkMatte :: C.Color -> Double -> Double -> Material
mkMatte cd kd ka = Material shade
  where
    ambient = BRDF.mkLambertian cd ka
    diffuse = BRDF.mkLambertian cd kd
    shade sr lights shadowTests =
      let
        wo = -(Ray.direction $ G.ray sr)
        ambientRadiance = BRDF.rho ambient wo
        lightFunc sr (Light directionFunc shadeFunc inShadowFunc) =
          let
            wi = directionFunc sr
            n = G.normal sr
            ndotwi = n `dot` wi
          in
            if ndotwi > 0 &&
               not (inShadowFunc (Ray.Ray (localHitPoint sr) wi) shadowTests) then
              BRDF.shade diffuse n wi wo * shadeFunc sr `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc sr) ambientRadiance lights

mkPhong :: C.Color -> Double -> Double -> Double -> Double -> Material
mkPhong cd kd ka ks exp = Material shade
  where
    ambient = BRDF.mkLambertian cd ka
    diffuse = BRDF.mkLambertian cd kd
    specular = BRDF.mkGlossySpecular ks exp
    shade sr lights shadowTests =
      let
        wo = -(Ray.direction $ G.ray sr)
        ambientRadiance = BRDF.rho ambient wo
        lightFunc sr (Light directionFunc shadeFunc inShadowFunc) =
          let
            wi = directionFunc sr
            n = G.normal sr
            ndotwi = n `dot` wi
          in
            if ndotwi > 0 &&
               not (inShadowFunc (Ray.Ray (localHitPoint sr) wi) shadowTests) then
              (BRDF.shade diffuse n wi wo + BRDF.shade specular n wi wo) * shadeFunc sr `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc sr) ambientRadiance lights

