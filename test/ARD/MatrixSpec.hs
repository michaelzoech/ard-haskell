module ARD.MatrixSpec where

import ARD.Matrix as Matrix
import ARD.Vector

import ARD.TestHelper

import Prelude hiding (length)
import Test.Hspec

spec :: Spec
spec = describe "Matrix" $ do
  it "identity * identify = identify" $
    Matrix.mul identity identity `shouldBe` identity
  it "rotate vector by 90 degrees around x axis" $
    transformVector (rotateX 90) (Vector3 0 1 0) `shouldBeClose` Vector3 0 0 1
  it "rotate vector by 90 degrees around y axis" $
    transformVector (rotateY 90) (Vector3 0 0 1) `shouldBeClose` Vector3 1 0 0
  it "rotate vector by 90 degrees around z axis" $
    transformVector (rotateZ 90) (Vector3 1 0 0) `shouldBeClose` Vector3 0 1 0
  it "translate point" $
    transformPoint (translate (Vector3 1 2 3)) (Vector3 4 5 6) `shouldBe` Vector3 5 7 9
  it "translate vector should have no effect" $
    transformVector (translate (Vector3 1 2 3)) (Vector3 4 5 6) `shouldBe` Vector3 4 5 6

