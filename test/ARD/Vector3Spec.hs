module ARD.Vector3Spec where

import Prelude hiding (length)
import ARD.TestHelper
import ARD.Vector3 as Vector3
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = describe "Vector3" $ do
  it "unit vector has length 1.0" $
    Vector3.length (Vector3 1.0 0.0 0.0) `shouldBe` 1.0
  it "plus example" $
    Vector3 1 2 3 `plus` Vector3 11 12 13 == Vector3 12 14 16
  it "v minus v == 0" $ property $
    \v -> v `minus` v == Vector3 0 0 0
  it "v plus v == 2v" $ property
    (\v -> v `plus` v == v `multiply` 2)
  it "dot product of perpendicular vectors is 0" $ property $
    \d -> Vector3 d 0 0 `dot` Vector3 0 d 0 == 0
  it "dot product of same vector is 1" $ property $
    \v -> (length v /= 0) ==> (normalize v `dot` normalize v `shouldBeClose` 1)
  it "normalized vector has length 1" $ property $
    \v -> (length v /= 0) ==> (length (normalize v) `shouldBeClose` 1)
  it "result of cross product is perpendicular" $ property $
    \u v -> (length u /= 0) && (length v /= 0) ==> normalize (u `cross` v) `dot` normalize u `shouldBeClose` 0

