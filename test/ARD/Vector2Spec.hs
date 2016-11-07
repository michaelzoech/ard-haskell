module ARD.Vector2Spec where

import Prelude hiding (length)
import ARD.TestHelper
import ARD.Vector2 as Vector2
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Vector2" $ do
  it "unit vector has length 1.0" $
    length (Vector2 1.0 0.0) `shouldBe` 1.0
  it "plus example" $
    Vector2 1 2 + Vector2 3 4 `shouldBe` Vector2 4 6
  it "minus example" $
    Vector2 1 2 - Vector2 3 4 `shouldBe` Vector2 (-2) (-2)
  it "v plus v == 2v" $ property
    (\v -> v + v == v `multiply` 2)
  it "normalized vector has length 1" $ property $
    \v -> (length v /= 0) ==> (length (normalize v) `shouldBeClose` 1)

