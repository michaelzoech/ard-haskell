module ARD.ColorSpec where

import ARD.Color

import ARD.TestHelper
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Color" $ do
  it "encodes a color to bgr little to high endian" $
    encodeColorToRGBWord32 (RGB 0.0 0.5 1.0) `shouldBe` 0x000080ff
  it "clamps values to min 0, max 1" $ do
    clampColor (RGB (-0.1) 0.5 1.1) `shouldBe` (RGB 0 0.5 1)

