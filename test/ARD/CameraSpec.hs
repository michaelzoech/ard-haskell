module ARD.CameraSpec where

import ARD.Camera
import ARD.Vector3

import ARD.TestHelper
import Test.Hspec

spec :: Spec
spec = describe "Camera" $ do
  orthographicCameraSpec
  pinholeCameraSpec

orthographicCameraSpec :: Spec
orthographicCameraSpec = describe "OrthographicCamera" $ do
  it "untransformed camera matches world axes" $ do
    let
      camera = makeOrthographicCamera (Vector3 0 0 0) (Vector3 0 0 (-1)) (Vector3 0 1 0)
      (u,v,w) = orthographicUVW camera
    u `shouldBe` Vector3 1 0 0
    v `shouldBe` Vector3 0 1 0
    w `shouldBe` Vector3 0 0 1

pinholeCameraSpec :: Spec
pinholeCameraSpec = describe "PinholeCamera" $ do
  it "untransformed camera matches world axes" $ do
    let
      camera = makePinholeCamera (Vector3 0 0 0) (Vector3 0 0 (-1)) (Vector3 0 1 0) 100
      (u,v,w) = pinholeUVW camera
    u `shouldBe` Vector3 1 0 0
    v `shouldBe` Vector3 0 1 0
    w `shouldBe` Vector3 0 0 1

