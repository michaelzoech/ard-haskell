module ARD.CameraSpec where

import ARD.Camera
import ARD.Vector

import Test.Hspec

spec :: Spec
spec = describe "Camera" $ do
  orthographicCameraSpec
  pinholeCameraSpec

orthographicCameraSpec :: Spec
orthographicCameraSpec = describe "OrthographicCamera" $ do
  it "untransformed camera matches world axes" $ do
    let
      camera = mkOrthographic (Vector3 0 0 0) (Vector3 0 0 (-1)) (Vector3 0 1 0)
      (u,v,w) = uvw camera
    u `shouldBe` Vector3 1 0 0
    v `shouldBe` Vector3 0 1 0
    w `shouldBe` Vector3 0 0 1

pinholeCameraSpec :: Spec
pinholeCameraSpec = describe "PinholeCamera" $ do
  it "untransformed camera matches world axes" $ do
    let
      camera = mkPinhole (Vector3 0 0 0) (Vector3 0 0 (-1)) (Vector3 0 1 0) 100
      (u,v,w) = uvw camera
    u `shouldBe` Vector3 1 0 0
    v `shouldBe` Vector3 0 1 0
    w `shouldBe` Vector3 0 0 1

