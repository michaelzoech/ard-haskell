module ARD.Material
  ( Material(..)
  , ShadeInfo(..)
  , mkMatte
  , mkNormal
  , mkPhong
  ) where

import ARD.Color as C
import qualified ARD.BRDF as BRDF
import ARD.Light as Light
import ARD.Ray as Ray
import ARD.Rendering
import ARD.Vector

type ShadeFunc = RenderContext -> ShadeInfo -> [Light.Light] -> Light -> [Ray -> Maybe Double] -> Color

data ShadeInfo = ShadeInfo
  { shadePoint :: Vector3
  , shadeNormal :: Vector3
  , shadeOutgoingRay :: Ray
  }

data Material
  = Material ShadeFunc

mkNormal :: Material
mkNormal = Material shade
  where
    shade renderContext si lights ambientLight shadowTests =
      let
        Vector3 x y z = abs $ shadeNormal si
      in
        C.RGB x y z

mkMatte :: C.Color -> Double -> Double -> Material
mkMatte cd kd ka = Material shade
  where
    ambient = BRDF.mkLambertian cd ka
    diffuse = BRDF.mkLambertian cd kd
    shade renderContext si lights ambientLight shadowTests =
      let
        wo = -(Ray.direction $ shadeOutgoingRay si)
        n = shadeNormal si
        point = shadePoint si
        ambientRadiance = BRDF.rho ambient wo * (Light.incidenceRadianceFunc ambientLight) renderContext point n shadowTests
        lightFunc (Light directionFunc incidenceRadiance inShadowFunc) =
          let
            wi = directionFunc renderContext point n
            ndotwi = n `dot` wi
          in
            if ndotwi > 0 &&
               not (inShadowFunc (Ray.Ray (shadePoint si) wi) shadowTests) then
              BRDF.shade diffuse n wi wo * incidenceRadiance renderContext point n shadowTests `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc) ambientRadiance lights

mkPhong :: C.Color -> Double -> Double -> Double -> Double -> Material
mkPhong cd kd ka ks exp = Material shade
  where
    ambient = BRDF.mkLambertian cd ka
    diffuse = BRDF.mkLambertian cd kd
    specular = BRDF.mkGlossySpecular ks exp
    shade renderContext si lights ambientLight shadowTests =
      let
        wo = -(Ray.direction $ shadeOutgoingRay si)
        n = shadeNormal si
        point = shadePoint si
        ambientRadiance = BRDF.rho ambient wo * (Light.incidenceRadianceFunc ambientLight) renderContext point n shadowTests
        lightFunc (Light directionFunc incidenceRadiance inShadowFunc) =
          let
            wi = directionFunc renderContext point n
            ndotwi = n `dot` wi
          in
            if ndotwi > 0 &&
               not (inShadowFunc (Ray.Ray (shadePoint si) wi) shadowTests) then
              (BRDF.shade diffuse n wi wo + BRDF.shade specular n wi wo) * incidenceRadiance renderContext point n shadowTests `C.mul` ndotwi
            else
              C.RGB 0 0 0
      in
        foldr ((+) . lightFunc) ambientRadiance lights

