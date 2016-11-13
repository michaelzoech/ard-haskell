module ARD.BRDF
  ( BRDF(..)
  , mkGlossySpecular
  , mkLambertian
  , ShadeFunc
  , RhoFunc
  ) where

import qualified ARD.Color as C
import ARD.Vector

-- | Calculates the reflected radiance of an incoming direction to a reflected direction.
-- Input: Surface normal -> Incoming direction -> Reflected direction
type ShadeFunc = ( Vector3 -> Vector3 -> Vector3 -> C.Color)

type RhoFunc = (Vector3 -> C.Color)

-- | Bidirectional reflectance distribution function.
-- Describes how light is reflected at a surface.
data BRDF
  = BRDF {
    shade :: ShadeFunc
  , rho :: RhoFunc
  }

mkLambertian :: C.Color -> Double -> BRDF
mkLambertian cd kd = BRDF shade rho
  where
    shade n wi wo = cd `C.mul` (kd * (1 / pi))
    rho wo = cd `C.mul` kd

mkGlossySpecular :: Double -> Double -> BRDF
mkGlossySpecular ks exp = BRDF shade rho
  where
    shade n wi wo =
      let
        ndotwi = n `dot` wi
        r = (-wi) + n `mul` (2 * ndotwi)
        rdotwo = r `dot` wo
        rad = if rdotwo > 0 then ks * (rdotwo ** exp) else 0
      in
        C.RGB rad rad rad
    rho wo = C.RGB 0 0 0

