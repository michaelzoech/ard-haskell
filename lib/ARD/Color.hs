module ARD.Color where

import Data.Bits
import Data.Word

data Color
  = RGB Double Double Double
  deriving (Eq, Show)

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

clampColor :: Color -> Color
clampColor (RGB r g b) =
  let
    clamp d = min 1 (max 0 d)
  in
    RGB (clamp r) (clamp g) (clamp b)

