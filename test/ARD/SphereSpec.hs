module ARD.SphereSpec where

import ARD.Geometric
import ARD.Ray
import ARD.Sphere
import ARD.Vector3

import ARD.TestHelper
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Sphere" $ do
  it "hit point" $
    hasHitPoint (Sphere (Vector3 2 0 0) 1) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 0 0)) (Vector3 1 0 0)
  it "not intersecting ray" $ do
    hit (Sphere (Vector3 1 1 1) 1) (Ray (Vector3 (-1) (-1) (-1)) (Vector3 1 0 0)) `shouldBe` Nothing
  it "positive ray starting inside" $ do
    hasHitPoint (Sphere (Vector3 1 0 0) 1) (Ray (Vector3 1 0 0) (normalize $ Vector3 1 0 0)) (Vector3 2 0 0)
  it "negative ray starting inside" $ do
    hasHitPoint (Sphere (Vector3 1 0 0) 1) (Ray (Vector3 1 0 0) (normalize $ Vector3 (-1) 0 0)) (Vector3 0 0 0)

