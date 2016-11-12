module ARD.VectorSpec where

import ARD.Vector

import ARD.TestHelper

import Prelude hiding (length)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Vector" $ do
  vector2Spec
  vector3Spec

vector2Spec :: Spec
vector2Spec = describe "Vector2" $ do
  it "unit vector has length 1.0" $
    length (Vector2 1.0 0.0) `shouldBe` 1.0
  it "plus example" $
    Vector2 1 2 + Vector2 3 4 `shouldBe` Vector2 4 6
  it "minus example" $
    Vector2 1 2 - Vector2 3 4 `shouldBe` Vector2 (-2) (-2)
  it "v plus v == 2v" $ property
    (\(v :: Vector2) -> v + v == v `mul` 2)
  it "normalized vector has length 1" $ property $
    \(v :: Vector2) -> (length v /= 0) ==> (length (normalize v) `shouldBeClose` 1)

vector3Spec :: Spec
vector3Spec = describe "Vector3" $ do
  it "unit vector has length 1.0" $
    length (Vector3 1.0 0.0 0.0) `shouldBe` 1.0
  it "plus example" $
    Vector3 1 2 3 + Vector3 11 12 13 == Vector3 12 14 16
  it "v minus v == 0" $ property $
    \v -> v - v == Vector3 0 0 0
  it "v plus v == 2v" $ property
    (\(v :: Vector3) -> v + v == v `mul` 2)
  it "dot product of perpendicular vectors is 0" $ property $
    \d -> Vector3 d 0 0 `dot` Vector3 0 d 0 == 0
  it "dot product of same vector is 1" $ property $
    \(v :: Vector3) -> (length v /= 0) ==> (normalize v `dot` normalize v `shouldBeClose` 1)
  it "normalized vector has length 1" $ property $
    \(v :: Vector3) -> (length v /= 0) ==> (length (normalize v) `shouldBeClose` 1)
  it "result of cross product is perpendicular" $ property $
    \(u :: Vector3) (v :: Vector3) -> (length u /= 0) && (length v /= 0) ==> normalize (u `cross` v) `dot` normalize u `shouldBeClose` 0

