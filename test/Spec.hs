import Test.Hspec

import qualified ARD.CameraSpec
import qualified ARD.ColorSpec
import qualified ARD.MatrixSpec
import qualified ARD.ParserSpec
import qualified ARD.SamplerSpec
import qualified ARD.ShapeSpec
import qualified ARD.VectorSpec

main :: IO ()
main = hspec $ do
  ARD.CameraSpec.spec
  ARD.ColorSpec.spec
  ARD.MatrixSpec.spec
  ARD.ParserSpec.spec
  ARD.SamplerSpec.spec
  ARD.ShapeSpec.spec
  ARD.VectorSpec.spec

