module ARD.Material
  ( Material(..)
  , ShadeInfo(..)
  , shade
  ) where

import ARD.Color as C
import qualified ARD.BRDF as BRDF
import ARD.Light as Light
import ARD.Ray as Ray
import ARD.Rendering
import ARD.Vector

data ShadeInfo = ShadeInfo
  { shadePoint :: Vector3
  , shadeNormal :: Vector3
  , shadeOutgoingRay :: Ray
  }

data Material
  = Normal
  | Matte !C.Color !Double !Double
  | Phong !C.Color !Double !Double !Double !Double


shade :: Material -> RenderContext -> ShadeInfo -> [Light.Light] -> Light -> [Ray -> Maybe Double] -> Color
shade Normal renderContext si lights ambientLight shadowTests = C.RGB x y z
  where
    Vector3 x y z = abs $ shadeNormal si
shade (Matte cd kd ka) renderContext si lights ambientLight shadowTests = foldr ((+) . lightFunc) ambientRadiance lights
  where
    ambient = BRDF.Lambertian cd ka
    diffuse = BRDF.Lambertian cd kd
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
shade (Phong cd kd ka ks exp) renderContext si lights ambientLight shadowTests = foldr ((+) . lightFunc) ambientRadiance lights
  where
    ambient = BRDF.Lambertian cd ka
    diffuse = BRDF.Lambertian cd kd
    specular = BRDF.GlossySpecular ks exp
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

