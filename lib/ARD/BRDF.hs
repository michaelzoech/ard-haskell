module ARD.BRDF
  ( BRDF(..)
  , rho
  , shade
  ) where

import qualified ARD.Color as C
import ARD.Vector

-- | Bidirectional reflectance distribution function.
-- Describes how light is reflected at a surface.
data BRDF
  = Lambertian !C.Color !Double
  | GlossySpecular !Double !Double

-- | Calculates the reflected radiance of an incoming direction to a reflected direction.
-- Input: Surface normal -> Incoming direction -> Reflected direction
shade :: BRDF -> Vector3 -> Vector3 -> Vector3 -> C.Color
shade (Lambertian diffuse kd) _ _ _ = diffuse `C.mul` (kd * (1 / pi))
shade (GlossySpecular ks exp) n wi wo = C.RGB rad rad rad
  where
    ndotwi = n `dot` wi
    r = (-wi) + n `mul` (2 * ndotwi)
    rdotwo = r `dot` wo
    rad = if rdotwo > 0 then ks * (rdotwo ** exp) else 0

rho :: BRDF -> Vector3 -> C.Color
rho (Lambertian diffuse kd) _ = diffuse `C.mul` kd
rho (GlossySpecular _ _) _ = C.RGB 0 0 0

