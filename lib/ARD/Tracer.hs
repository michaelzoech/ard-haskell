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
    ps = pixelSize vp
    width = horizontalResolution vp
    height = verticalResolution vp
    resX = (fromIntegral width) :: Double
    resY = (fromIntegral height) :: Double
    xy = [ (fromIntegral x, fromIntegral y) | y <- [0..height-1], x <- [0..width-1] ]
    pixelPos u total = ps * (u - 0.5 * (total - 1.0))
    direction = Vector3 0 0 (-1)
    rays = [ Ray (Vector3 (pixelPos x resX) (pixelPos y resY) 100) direction | (x,y) <- xy ]
  in
    map (traceRay world) rays

traceRay :: World -> Ray -> Color
traceRay world ray
  | hits == [] = backgroundColor world
  | otherwise = G.color $ G.shadeRecord nearestHit
  where
    objects = sceneObjects world
    hits = catMaybes $ map (flip hit ray) objects
    nearestHit = minimumBy (\a b ->G.tmin a `compare` G.tmin b) hits

