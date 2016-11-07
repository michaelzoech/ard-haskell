
import ARD.Bitmap
import ARD.Camera
import ARD.Color
import ARD.Sampler
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
    width = 800
    height = 600
    world = World
      { camera = SceneCamera $ makeOrthographicCamera (Vector3 100 50 100) (Vector3 0 0 0) (Vector3 0 1 0)
      , viewPlane = ViewPlane
        { horizontalResolution = width
        , verticalResolution = height
        , pixelSize = 1
        , pixelSampler = genRegularSampler 16
        }
      , sceneObjects =
        [ SceneObject $ Sphere (Vector3 (-100) 0 0) 40 (RGB 1 0 0)
        , SceneObject $ Sphere (Vector3 0 0 0) 40 (RGB 0 1 0)
        , SceneObject $ Sphere (Vector3 100 0 0) 40 (RGB 0 0 1)
        , SceneObject $ Sphere (Vector3 0 0 (-200)) 150 (RGB 0.2 0.2 0.2)
        ] :: [SceneObject]
      , backgroundColor = RGB 0 0 0
      }
    pixels = map clampColor . traceScene $ world
  in writeBitmapToFile width height pixels "out.bmp"

