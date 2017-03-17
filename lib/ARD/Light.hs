module ARD.Light
  ( Light(..)
  , direction
  , incidenceRadiance
  , inShadow
  ) where

import qualified ARD.Color as C
import qualified ARD.Ray as Ray
import ARD.Rendering
import qualified ARD.Sampler as Sampler
import ARD.Vector as V

import qualified Data.Maybe as Maybe

data Light
  = Ambient !C.Color !Double
  | AmbientOccluder !C.Color !Double !C.Color !Sampler.Sampler
  | Directional !Vector3 !C.Color !Double
  | Point !Vector3 !C.Color !Double

direction :: Light -> RenderContext -> Vector3 -> Vector3 -> Vector3
direction (Ambient color ls) _ _ _ = Vector3 0 0 0
direction (AmbientOccluder color ls minColor sampler) renderContext point normal =
  let
    w = normal
    v = normalize (w `cross` Vector3 0.0072 1.0 0.0034)
    u = v `cross` w
    pr = pixelRandom renderContext
    rr = rayNumber renderContext
    Vector3 x y z = Sampler.hemisphereSamples sampler !! (pr `mod` Sampler.numSets sampler) !! (rr `mod` Sampler.numSamples sampler)
  in
    normalize (u `mul` x + v `mul` y + w `mul` z)
direction (Directional invDirection _ _) _ _ _ = normalize invDirection
direction (Point location _ _) _ point _ = normalize (location - point)

incidenceRadiance :: Light -> RenderContext -> Vector3 -> Vector3 -> [Ray.Ray -> Maybe Double] -> C.Color
incidenceRadiance (Ambient color ls) _ _ _ _ = color `C.mul` ls
incidenceRadiance ao@(AmbientOccluder color ls minColor sampler) renderContext point normal fs =
  let
    ray = Ray.Ray { Ray.origin = point, Ray.direction = direction ao renderContext point normal }
    radiance = color `C.mul` ls
  in
    if inShadow ao ray fs then minColor * radiance else radiance
incidenceRadiance (Directional invDirection color ls) renderContext point normal fs = color `C.mul` ls
incidenceRadiance (Point location color ls) renderContext point normal fs = color `C.mul` ls

inShadow :: Light -> Ray.Ray -> [Ray.Ray -> Maybe Double] -> Bool
inShadow Ambient{} _ _ = False
inShadow AmbientOccluder{} ray fs = any (\f -> Maybe.isJust (f ray)) fs
inShadow Directional{} ray fs = any (\f -> Maybe.isJust (f ray)) fs
inShadow (Point location _ _) ray fs =
  let
    d = V.length (location - Ray.origin ray)
  in
    any (< d) $ Maybe.mapMaybe (\f -> f ray) fs

