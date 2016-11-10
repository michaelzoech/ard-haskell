module ARD.SphereSpec where

import ARD.Geometric
import ARD.Ray
import ARD.Sphere
import ARD.Vector
import Data.Maybe

import ARD.TestHelper
import Test.Hspec

spec :: Spec
spec = describe "Sphere" $ do
  it "hit point" $
    hasHitPoint (Sphere (Vector3 2 0 0) 1 dummyMaterial) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 0 0)) (Vector3 1 0 0)
  it "not intersecting ray" $
    isNothing (hit (Sphere (Vector3 1 1 1) 1 dummyMaterial) (Ray (Vector3 (-1) (-1) (-1)) (Vector3 1 0 0))) `shouldBe` True
  it "positive ray starting inside" $
    hasHitPoint (Sphere (Vector3 1 0 0) 1 dummyMaterial) (Ray (Vector3 1 0 0) (normalize $ Vector3 1 0 0)) (Vector3 2 0 0)
  it "negative ray starting inside" $
    hasHitPoint (Sphere (Vector3 1 0 0) 1 dummyMaterial) (Ray (Vector3 1 0 0) (normalize $ Vector3 (-1) 0 0)) (Vector3 0 0 0)

