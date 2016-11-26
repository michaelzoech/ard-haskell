import Test.Hspec

import qualified ARD.CameraSpec
import qualified ARD.ColorSpec
import qualified ARD.MatrixSpec
import qualified ARD.ParserSpec
import qualified ARD.PlaneSpec
import qualified ARD.SamplerSpec
import qualified ARD.SphereSpec
import qualified ARD.VectorSpec

main :: IO ()
main = hspec $ do
  ARD.CameraSpec.spec
  ARD.ColorSpec.spec
  ARD.MatrixSpec.spec
  ARD.ParserSpec.spec
  ARD.PlaneSpec.spec
  ARD.SamplerSpec.spec
  ARD.SphereSpec.spec
  ARD.VectorSpec.spec

