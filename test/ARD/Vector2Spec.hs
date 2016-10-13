module ARD.Vector2Spec where

import ARD.Vector2 as Vector2
import Test.Hspec

spec :: Spec
spec = describe "Vector2" $ do
  it "unit vector has length 1.0" $
    Vector2.length (Vector2 1.0 0.0) `shouldBe` 1.0

