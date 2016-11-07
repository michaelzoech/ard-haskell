
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
    red = RGB 1 0 0
    green = RGB 0 1 0
    blue = RGB 0 0 1
    darkGray = RGB 0.2 0.2 0.2
    world = World
      { camera = SceneCamera $ makePinholeCamera (Vector3 0 0 400) (Vector3 0 0 0) (Vector3 0 1 0) 400
      , viewPlane = ViewPlane
        { horizontalResolution = width
        , verticalResolution = height
        , pixelSize = 0.5
        , pixelSampler = genRegularSampler 16
        }
      , sceneObjects =
        [ SceneObject $ Sphere (Vector3 (-100) 0 0) 40 red
        , SceneObject $ Sphere (Vector3 0 0 0) 40 green
        , SceneObject $ Sphere (Vector3 100 0 0) 40 blue
        , SceneObject $ Sphere (Vector3 (-100) 0 (-100)) 40 red
        , SceneObject $ Sphere (Vector3 0 0 (-100)) 40 green
        , SceneObject $ Sphere (Vector3 100 0 (-100)) 40 blue
        , SceneObject $ Sphere (Vector3 (-100) 0 (-200)) 40 red
        , SceneObject $ Sphere (Vector3 0 0 (-200)) 40 green
        , SceneObject $ Sphere (Vector3 100 0 (-200)) 40 blue
        , SceneObject $ Sphere (Vector3 0 0 (-350)) 100 darkGray
        ] :: [SceneObject]
      , backgroundColor = RGB 0 0 0
      }
    pixels = map clampColor . traceScene $ world
  in writeBitmapToFile width height pixels "out.bmp"

