import Test.Hspec

import qualified ARD.ColorSpec
import qualified ARD.PlaneSpec
import qualified ARD.SamplerSpec
import qualified ARD.SphereSpec
import qualified ARD.Vector2Spec
import qualified ARD.Vector3Spec

main :: IO ()
main = hspec $ do
  ARD.ColorSpec.spec
  ARD.PlaneSpec.spec
  ARD.SamplerSpec.spec
  ARD.SphereSpec.spec
  ARD.Vector2Spec.spec
  ARD.Vector3Spec.spec

