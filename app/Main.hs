
import ARD.Bitmap
import ARD.Color
import ARD.Sphere
import ARD.Tracer
import ARD.Vector3
import ARD.ViewPlane
import ARD.World
import Data.Word
import System.Environment

main :: IO ()
main =
  let
    width = 1024
    height = 768
    world = World
      { viewPlane = ViewPlane
        { horizontalResolution = width
        , verticalResolution = height
        , pixelSize = 0.5
        }
      , sceneObjects =
        [ SceneObject $ Sphere (Vector3 (-100) 0 0) 40 (RGB 1 0 0)
        , SceneObject $ Sphere (Vector3 0 0 0) 40 (RGB 0 1 0)
        , SceneObject $ Sphere (Vector3 100 0 0) 40 (RGB 0 0 1)
        ] :: [SceneObject]
      , backgroundColor = RGB 0 0 0
      }
    pixels = [ clampColor c | c <- traceScene world ]
  in writeBitmapToFile width height pixels "out.bmp"

