module ARD.Tracer
  ( traceScene
  ) where

import ARD.Camera
import ARD.Color
import ARD.Geometric as G
import ARD.Material
import ARD.Ray
import ARD.Sampler
import ARD.Vector as Vector
import ARD.ViewPlane
import ARD.World

import Control.Parallel.Strategies as P
import Data.List (minimumBy)
import Data.Maybe (catMaybes, mapMaybe)

traceScene :: World -> [Color]
traceScene world =
  let
    cam = camera world
    vp = viewPlane world
    width = horizontalResolution vp
    height = verticalResolution vp
    resX = fromIntegral width
    resY = fromIntegral height
    ps = pixelSize vp
    samples = unitSquareSamples $ pixelSampler vp
    xy = [ (fromIntegral x, fromIntegral y) | y <- [0..height-1], x <- [0..width-1] ]
    direction = Vector3 0 0 (-1)
    raysPerPixel = [ generateRays samples x y | (x,y) <- xy ]
    pixelPos u v total = ps * (fromIntegral u - 0.5 * total + v)
    generateRays samples x y = map (\(Vector2 x' y') -> generateRay cam (Vector2 (pixelPos x x' resX) (pixelPos y y' resY))) samples
    colors = map (tracePixel world) raysPerPixel
  in
    colors `P.using` P.parListChunk height P.rpar

tracePixel :: World -> [Ray] -> Color
tracePixel world rays =
  let
    color = sum $ map (traceRay world) rays
    numRays = fromIntegral $ Prelude.length rays
  in
    color `forChannels` (/ numRays)

traceRay :: World -> Ray -> Color
traceRay world ray
  | null hits = backgroundColor world
  | otherwise =
    let
      (G.Material shadeFunc) = (G.material $ G.shadeRecord nearestHit)
      shadowTests = map G.shadowHit (sceneObjects world)
    in
      shadeFunc (G.shadeRecord nearestHit) (lights world) shadowTests
  where
    objects = sceneObjects world
    hits = mapMaybe (`hit` ray) objects
    nearestHit = minimumBy (\a b ->G.tmin a `compare` G.tmin b) hits

