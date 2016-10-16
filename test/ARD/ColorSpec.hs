{-# LANGUAGE ScopedTypeVariables #-}
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
  it "satisfies abs signum law" $ property $
    \(c :: Color) -> abs c * signum c `shouldBe` c
  it "supports +" $ do
    (RGB 1 2 3) + (RGB 4 5 6) `shouldBe` (RGB 5 7 9)
  it "supports *" $ do
    (RGB 1 2 3) * (RGB 4 5 6) `shouldBe` (RGB 4 10 18)
  it "negate twice is id" $ property $
    \(c :: Color) -> (negate . negate) c `shouldBe` c
  it "fromInteger reads as BGR little to high endian" $
    (fromInteger 0x00ff8040 :: Color) `shouldBe` (RGB 1.0 (128/255) (64/255))

