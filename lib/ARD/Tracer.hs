module ARD.Tracer
  ( traceScene
  ) where

import ARD.Color
import ARD.Geometric as G
import ARD.Ray
import ARD.Vector3
import ARD.ViewPlane
import ARD.World
import Data.List (minimumBy)
import Data.Maybe (catMaybes)

traceScene :: World -> [Color]
traceScene world =
  let
    vp = viewPlane world
    width = horizontalResolution vp
    height = verticalResolution vp
    xy = [ (fromIntegral x, fromIntegral y) | y <- [0..height-1], x <- [0..width-1] ]
    direction = Vector3 0 0 (-1)
    generateRays = map (\(x,y) -> Ray (Vector3 x y 100) direction)
    raysPerPixel = [ generateRays $ (subPixelGenerator vp) vp x y | (x,y) <- xy ]
  in
    map (tracePixel world) raysPerPixel

tracePixel :: World -> [Ray] -> Color
tracePixel world rays =
  let
    color = sum $ map (traceRay world) rays
    numRays = fromIntegral $ Prelude.length rays
  in
    color `forChannels` (/ numRays)

traceRay :: World -> Ray -> Color
traceRay world ray
  | hits == [] = backgroundColor world
  | otherwise = G.color $ G.shadeRecord nearestHit
  where
    objects = sceneObjects world
    hits = catMaybes $ map (flip hit ray) objects
    nearestHit = minimumBy (\a b ->G.tmin a `compare` G.tmin b) hits

