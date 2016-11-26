module ARD.BoxSpec where

import ARD.Ray
import ARD.Box
import ARD.Vector

import ARD.TestHelper

import Test.Hspec

spec :: Spec
spec = describe "Box" $
  it "hit point tests" $ do
    let
      box = Box
        { center = Vector3 10 0 0
        , vu = Vector3 1 0 0
        , vv = Vector3 0 1 0
        , vw = Vector3 0 0 1
        , material = dummyMaterial
        }
    hasHitPoint box (Ray (Vector3 0 0 0) (Vector3 1 0 0)) (Vector3 9 0 0)
    hasHitPoint box (Ray (Vector3 8 0 0) (Vector3 1 0 0)) (Vector3 9 0 0)
    hasHitPoint box (Ray (Vector3 10 2 0) (Vector3 0 (-1) 0)) (Vector3 10 1 0)
    hasNoHitPoint box (Ray (Vector3 10 2 0) (Vector3 0 1 0))
    hasNoHitPoint box (Ray (Vector3 11.1 1 0) (Vector3 1 0 0))

