module ARD.Light
  ( Light(..)
  , incidenceRadianceFunc
  , mkAmbient
  , mkAmbientOccluder
  , mkDirectional
  , mkPoint
  ) where

import qualified ARD.Color as C
import qualified ARD.Ray as Ray
import ARD.Rendering
import qualified ARD.Sampler as Sampler
import ARD.Vector as V

import qualified Data.Maybe as Maybe

type DirectionFunc = RenderContext-> Vector3 -> Vector3 -> Vector3
type IncidenceRadiance= RenderContext -> Vector3 -> Vector3 -> [Ray.Ray -> Maybe Double] -> C.Color
type InShadowFunc = Ray.Ray -> [Ray.Ray -> Maybe Double] -> Bool

data Light
  = Light DirectionFunc IncidenceRadiance InShadowFunc

incidenceRadianceFunc :: Light -> IncidenceRadiance
incidenceRadianceFunc (Light _ f _ ) = f

mkAmbient :: C.Color -> Double -> Light
mkAmbient color ls = Light direction incidenceRadiance inShadow
  where
    direction renderContext point normal = Vector3 0 0 0
    incidenceRadiance renderContext point normal fs = color `C.mul` ls
    inShadow _ _ = False

mkAmbientOccluder :: C.Color -> Double -> C.Color -> Sampler.Sampler -> Light
mkAmbientOccluder color ls minColor sampler = Light direction incidenceRadiance inShadow
  where
    direction renderContext point normal =
      let
        w = normal
        v = normalize (w `cross` Vector3 0.0072 1.0 0.0034)
        u = v `cross` w
        pr = pixelRandom renderContext
        rr = rayNumber renderContext
        Vector3 x y z = Sampler.hemisphereSamples sampler !! (pr `mod` Sampler.numSets sampler) !! (rr `mod` Sampler.numSamples sampler)
      in
        normalize (u `mul` x + v `mul` y + w `mul` z)
    incidenceRadiance renderContext point normal fs =
      let
        ray = Ray.Ray { Ray.origin = point, Ray.direction = direction renderContext point normal }
        radiance = color `C.mul` ls
      in
        if inShadow ray fs then minColor * radiance else radiance
    inShadow ray = any (\f -> Maybe.isJust (f ray))

mkDirectional :: Vector3 -> C.Color -> Double -> Light
mkDirectional invDirection color ls = Light direction incidenceRadiance inShadow
  where
    normalizedInvDirection = normalize invDirection
    direction renderContext point normal = normalizedInvDirection
    incidenceRadiance renderContext point normal fs = color `C.mul` ls
    inShadow ray = any (\f -> Maybe.isJust (f ray))

mkPoint :: Vector3 -> C.Color -> Double -> Light
mkPoint location color ls = Light direction incidenceRadiance inShadow
  where
    direction renderContext point normal = normalize (location - point)
    incidenceRadiance renderContext point normal fs = color `C.mul` ls
    inShadow ray fs =
      let
        d = V.length (location - Ray.origin ray)
      in
        any (< d) $ Maybe.mapMaybe (\f -> f ray) fs

