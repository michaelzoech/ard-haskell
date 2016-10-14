module ARD.ViewPlane where

data ViewPlane
  = ViewPlane
  { horizontalResolution :: Int
  , verticalResolution :: Int
  , pixelSize :: Double
  }
  deriving (Eq, Show)

