module ARD.Color where

import Data.Bits
import Data.Word
import Prelude hiding (div)

data Color
  = RGB !Double !Double !Double
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
mul c d = c `forChannels` (*d)

div :: Color -> Double -> Color
div c d = c `forChannels` (/d)

-- | Map possible out of gamut color into 0.0-1.0 range
maxToOne :: Color -> Color
maxToOne c@(RGB r g b) =
  let
    maxValue = maximum [r, g, b]
  in
    if maxValue > 1 then
      c `div` maxValue
    else
      c

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

