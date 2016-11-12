module ARD.ViewPlane where

import ARD.Sampler

data ViewPlane
  = ViewPlane
  { horizontalResolution :: Int
  , verticalResolution :: Int
  , pixelSize :: Double
  , pixelSampler :: Sampler
  }

