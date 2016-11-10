module ARD.Color where

import Data.Bits
import Data.Word

data Color
  = RGB Double Double Double
  deriving (Eq, Show)

instance Num Color where
  (+) (RGB r g b) (RGB r' g' b') = RGB (r+r') (g+g') (b+b')
  (*) (RGB r g b) (RGB r' g' b') = RGB (r*r') (g*g') (b*b')
  (-) (RGB r g b) (RGB r' g' b') = RGB (r-r') (g-g') (b-b')
  negate c = c `forChannels` negate
  abs c = c `forChannels` abs
  signum c = c `forChannels` signum
  fromInteger i =
    let
      i' = fromInteger i :: Word32
      b = fromInteger $ toInteger (0xff .&. i')
      g = fromInteger $ toInteger $ (0xff00 .&. i') `shiftR` 8
      r = fromInteger $ toInteger $ (0xff0000 .&. i') `shiftR` 16
    in
      RGB (r / 255) (g / 255) (b / 255)

mul :: Color -> Double -> Color
mul (RGB r g b) d = RGB (r*d) (g*d) (b*d)

-- | Encode Color into Word32.
-- The ordering of the channels from little to highest is BGR.
encodeColorToRGBWord32 :: Color -> Word32
encodeColorToRGBWord32 (RGB r g b) =
  let
    enc :: Double -> Word32
    enc d = (fromInteger $ round (d * 255)) :: Word32
    b' = (0xff .&. enc b)
    g' = (0xff .&. enc g) `shiftL` 8
    r' = (0xff .&. enc r) `shiftL` 16
  in r' .|. g' .|. b'

-- | Clamp Color to the standard boundaries.
clampColor :: Color -> Color
clampColor c = c `forChannels` (min 1 . max 0 )

-- | Modify each channel by applying the given function.
forChannels :: Color -> (Double -> Double) -> Color
forChannels (RGB r g b) f = RGB (f r) (f g) (f b)

