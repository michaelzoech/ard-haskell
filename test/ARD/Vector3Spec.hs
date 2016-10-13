module ARD.Vector3Spec where

import ARD.Vector3 as Vector3
import Test.Hspec

spec :: Spec
spec = describe "Vector3" $ do
  it "unit vector has length 1.0" $
    Vector3.length (Vector3 1.0 0.0 0.0) `shouldBe` 1.0

