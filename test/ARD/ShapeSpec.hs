module ARD.ShapeSpec where

import ARD.Ray
import ARD.Shape
import ARD.Vector

import ARD.TestHelper

import Data.Maybe
import Test.Hspec

spec :: Spec
spec = describe "Shape" $ do
  boxSpec
  planeSpec
  sphereSpec

boxSpec :: Spec
boxSpec = describe "Box" $
  it "hit point tests" $ do
    let
      center = Vector3 10 0 0
      vu = Vector3 1 0 0
      vv = Vector3 0 1 0
      vw = Vector3 0 0 1
      material = dummyMaterial
      box = Box center vu vv vw material
    hasHitPoint box (Ray (Vector3 0 0 0) (Vector3 1 0 0)) (Vector3 9 0 0)
    hasHitPoint box (Ray (Vector3 8 0 0) (Vector3 1 0 0)) (Vector3 9 0 0)
    hasHitPoint box (Ray (Vector3 10 2 0) (Vector3 0 (-1) 0)) (Vector3 10 1 0)
    hasNoHitPoint box (Ray (Vector3 10 2 0) (Vector3 0 1 0))
    hasNoHitPoint box (Ray (Vector3 11.1 1 0) (Vector3 1 0 0))

planeSpec :: Spec
planeSpec = describe "Plane" $ do
  it "hit point" $
    hasHitPoint (Plane (Vector3 1 0 0) (Vector3 1 0 0) dummyMaterial) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 1 0)) (Vector3 1 1 0)

sphereSpec :: Spec
sphereSpec = describe "Sphere" $ do
  it "hit point" $
    hasHitPoint (Sphere (Vector3 2 0 0) 1 dummyMaterial) (Ray (Vector3 0 0 0) (normalize $ Vector3 1 0 0)) (Vector3 1 0 0)
  it "not intersecting ray" $
    isNothing (hit (Sphere (Vector3 1 1 1) 1 dummyMaterial) (Ray (Vector3 (-1) (-1) (-1)) (Vector3 1 0 0))) `shouldBe` True
  it "positive ray starting inside" $
    hasHitPoint (Sphere (Vector3 1 0 0) 1 dummyMaterial) (Ray (Vector3 1 0 0) (normalize $ Vector3 1 0 0)) (Vector3 2 0 0)
  it "negative ray starting inside" $
    hasHitPoint (Sphere (Vector3 1 0 0) 1 dummyMaterial) (Ray (Vector3 1 0 0) (normalize $ Vector3 (-1) 0 0)) (Vector3 0 0 0)

