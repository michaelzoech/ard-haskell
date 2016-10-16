module ARD.ViewPlane where

import ARD.Ray
import ARD.Vector3

data ViewPlane
  = ViewPlane
  { horizontalResolution :: Int
  , verticalResolution :: Int
  , pixelSize :: Double
  , numSamples :: Int
  , subPixelGenerator :: ViewPlane -> Int -> Int -> [(Double, Double)]
  }

standardSampling :: ViewPlane -> Int -> Int -> [(Double, Double)]
standardSampling vp x y =
  let
    ps = pixelSize vp
    width = horizontalResolution vp
    height = verticalResolution vp
    resX = (fromIntegral width) :: Double
    resY = (fromIntegral height) :: Double
    pixelPos u total = ps * ((fromIntegral u) - 0.5 * (total - 1.0))
  in
    [(pixelPos x resX, pixelPos y resY)]

regularSampling :: ViewPlane -> Int -> Int -> [(Double, Double)]
regularSampling vp x y =
  let
    ps = pixelSize vp
    width = horizontalResolution vp
    height = verticalResolution vp
    resX = (fromIntegral width) :: Double
    resY = (fromIntegral height) :: Double
    n = sqrt $ fromIntegral $ numSamples vp
    pixelPos u v total = ps * ((fromIntegral u) - 0.5 * total + (v + 0.5) / n)
  in
    [ (pixelPos x p resX, pixelPos y p resY) | p <- [0..(n-1)] ]

