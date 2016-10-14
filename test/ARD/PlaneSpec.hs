module ARD.PlaneSpec where

import ARD.Geometric
import ARD.Plane
import ARD.Ray
import ARD.Vector3
import ARD.TestHelper
import Test.Hspec
import Test.QuickCheck

hasHitPoint :: Plane -> Ray -> Point3 -> Expectation
hasHitPoint plane ray hitPoint =
  case hit plane ray of
    Just hitResult ->
      let shadeRec = shadeRecord hitResult
      in (localHitPoint shadeRec) `shouldBe` hitPoint
    _ -> expectationFailure "No hit point found"

spec :: Spec
spec = describe "Plane" $ do
  it "hit point" $
    hasHitPoint (Plane (Vector3 1 0 0) (Vector3 1 0 0)) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 1 0)) (Vector3 1 1 0)
