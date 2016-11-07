module ARD.PlaneSpec where

import ARD.Color
import ARD.Plane
import ARD.Ray
import ARD.Vector3
import ARD.TestHelper
import Test.Hspec

spec :: Spec
spec = describe "Plane" $ do
  it "hit point" $
    hasHitPoint (Plane (Vector3 1 0 0) (Vector3 1 0 0) (RGB 0 0 0)) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 1 0)) (Vector3 1 1 0)
