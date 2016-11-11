
import ARD.Bitmap
import ARD.Camera
import ARD.Color
import qualified ARD.Light as Light
import qualified ARD.Material as Material
import ARD.Plane
import ARD.Sampler
import ARD.Sphere
import ARD.Tracer
import ARD.Vector
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
    gray = RGB 0.5 0.5 0.5
    white = RGB 1 1 1
    redMatte = Material.mkMatte red 1 0.2
    greenMatte = Material.mkMatte green 1 0.2
    blueMatte = Material.mkMatte blue 1 0.2
    grayMatte = Material.mkMatte gray 1 0.2
    redPhong = Material.mkPhong red 1 0.2 white 5
    greenPhong = Material.mkPhong green 1 0.2 white 20
    bluePhong = Material.mkPhong blue 1 0.2 white 150
    world = World
      { camera = SceneCamera $ makePinholeCamera (Vector3 0 100 400) (Vector3 0 20 (-120)) (Vector3 0 1 0) 450
      , viewPlane = ViewPlane
        { horizontalResolution = width
        , verticalResolution = height
        , pixelSize = 0.5
        , pixelSampler = genRegularSampler 16
        }
      , sceneObjects =
        [ SceneObject $ Sphere (Vector3 (-100) (-40) 0) 40 redPhong
        , SceneObject $ Sphere (Vector3 0 (-40) 0) 40 greenPhong
        , SceneObject $ Sphere (Vector3 100 (-40) 0) 40 bluePhong
        , SceneObject $ Sphere (Vector3 (-100) 0 (-100)) 40 redMatte
        , SceneObject $ Sphere (Vector3 0 0 (-100)) 40 greenMatte
        , SceneObject $ Sphere (Vector3 100 0 (-100)) 40 blueMatte
        , SceneObject $ Sphere (Vector3 (-100) 40 (-200)) 40 redMatte
        , SceneObject $ Sphere (Vector3 0 40 (-200)) 40 greenMatte
        , SceneObject $ Sphere (Vector3 100 40 (-200)) 40 blueMatte
        , SceneObject $ Sphere (Vector3 0 0 (-350)) 100 grayMatte
        , SceneObject $ Plane (Vector3 0 (-100) 0) (Vector3 0 1 0) grayMatte
        ] :: [SceneObject]
      , lights =
        [ Light.mkPoint (Vector3 70 50 100) white 1.0
        , Light.mkPoint (Vector3 (-70) 50 100) white 1.0
        , Light.mkDirectional (Vector3 0 1 0) white 0.8
        ]
      , backgroundColor = RGB 0 0 0
      }
    pixels = map maxToOne . traceScene $ world
  in writeBitmapToFile width height pixels "out.bmp"

