import Test.Hspec

import qualified ARD.Vector2Spec
import qualified ARD.Vector3Spec

main :: IO ()
main = hspec $ do
  ARD.Vector2Spec.spec
  ARD.Vector3Spec.spec

