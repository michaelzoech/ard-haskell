module ARD.PlaneSpec where

import ARD.Plane
import ARD.Ray
import ARD.Vector

import ARD.TestHelper

import Test.Hspec

spec :: Spec
spec = describe "Plane" $ do
  it "hit point" $
    hasHitPoint (Plane (Vector3 1 0 0) (Vector3 1 0 0) dummyMaterial) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 1 0)) (Vector3 1 1 0)

