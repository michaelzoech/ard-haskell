module ARD.Tracer
  ( traceScene
  ) where

import ARD.Camera
import ARD.Color
import ARD.Geometric as G
import ARD.Material
import ARD.Randomize
import ARD.Rendering
import ARD.Ray
import ARD.Sampler
import ARD.Vector as Vector
import ARD.ViewPlane
import ARD.World

import Control.Monad
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
    sampler = pixelSampler vp
    nSamples = numSamples sampler
    nSets = numSets sampler
    nPixels = width * height
    xy = [ (x,y) | y <- [0..height-1], x <- [0..width-1] ]
    random = randomState world
    (pixelRands, samplerRands) = fst $ flip runRandomized random $ do
      pixelRandoms <- getRandoms nPixels
      samplerRandoms <- getRandoms nPixels
      return (pixelRandoms, samplerRandoms)
    input = flip map (zip3 xy pixelRands samplerRands) $ \((x,y),pixelRandom, samplerRandom) ->
        let
          dx = fromIntegral x
          dy = fromIntegral y
          samples = unitSquareSamples sampler !! (samplerRandom `mod` nSets)
          pixelPos u v total = ps * (u - 0.5 * total + v)
        in
          flip map (zip [0..nSamples-1] samples) $ \(rayIndex, Vector2 x' y') ->
            let
              ray = generateRay cam (Vector2 (pixelPos dx x' resX) (pixelPos dy y' resY))
              context = RenderContext
                { pixelNumber = y*width + x
                , rayNumber = rayIndex
                , pixelRandom = pixelRandom
                }
            in
              (ray, context)
    colors = map (tracePixel world) input
  in
    colors `P.using` P.parListChunk width P.rdeepseq

tracePixel :: World -> [(Ray, RenderContext)] -> Color
tracePixel world rays =
  let
    color = sum $ map (traceRay world) rays
    numRays = fromIntegral $ Prelude.length rays
  in
    color `forChannels` (/ numRays)

traceRay :: World -> (Ray, RenderContext) -> Color
traceRay world (ray, renderContext)
  | null hits = backgroundColor world
  | otherwise =
    let
      (Material shadeFunc) = (G.material $ G.shadeRecord nearestHit)
      shadowTests = map shadowHit (sceneObjects world)
    in
      shadeFunc renderContext (shadeRecordToShadeInfo $ G.shadeRecord nearestHit) (lights world) (ambientLight world) shadowTests
  where
    objects = sceneObjects world
    hits = mapMaybe (`hit` ray) objects
    nearestHit = minimumBy (\a b ->G.tmin a `compare` G.tmin b) hits

shadeRecordToShadeInfo :: G.ShadeRecord -> ShadeInfo
shadeRecordToShadeInfo sr =
  ShadeInfo
    { shadePoint = G.localHitPoint sr
    , shadeNormal = G.normal sr
    , shadeOutgoingRay = G.ray sr
    }
