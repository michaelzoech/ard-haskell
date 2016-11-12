module ARD.BRDF
  ( BRDF(..)
  , mkGlossySpecular
  , mkLambertian
  ) where

import qualified ARD.Color as C
import qualified ARD.Geometric as G
import ARD.Vector

type ShadeFunc = (G.ShadeRecord -> Vector3 -> Vector3 -> C.Color)

type RhoFunc = (G.ShadeRecord -> Vector3 -> C.Color)

data BRDF
  = BRDF
  { shade :: ShadeFunc
  , rho :: RhoFunc
  }

mkLambertian :: C.Color -> Double -> BRDF
mkLambertian cd kd = BRDF shade rho
  where
    shade sr wi wo = cd `C.mul` (kd * (1 / pi))
    rho sr wo = cd `C.mul` kd

mkGlossySpecular :: Double -> Double -> BRDF
mkGlossySpecular ks exp = BRDF shade rho
  where
    shade sr wi wo =
      let
        ndotwi = G.normal sr `dot` wi
        r = (-wi) + G.normal sr `mul` (2 * ndotwi)
        rdotwo = r `dot` wo
        rad = if rdotwo > 0 then ks * (rdotwo ** exp) else 0
      in
        C.RGB rad rad rad
    rho sr wo = C.RGB 0 0 0

