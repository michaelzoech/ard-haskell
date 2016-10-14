import Test.Hspec

import qualified ARD.PlaneSpec
import qualified ARD.Vector2Spec
import qualified ARD.Vector3Spec

main :: IO ()
main = hspec $ do
  ARD.PlaneSpec.spec
  ARD.Vector2Spec.spec
  ARD.Vector3Spec.spec

