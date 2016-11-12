module ARD.Parser where

import qualified ARD.Camera as Camera
import qualified ARD.Color as Color
import qualified ARD.Geometric as Geometric
import qualified ARD.Light as Light
import qualified ARD.Material as Material
import qualified ARD.Plane as Plane
import qualified ARD.Sampler as Sampler
import qualified ARD.Sphere as Sphere
import qualified ARD.Vector as Vector
import qualified ARD.ViewPlane as ViewPlane
import qualified ARD.World as World

import Control.Applicative
import Numeric (readDec, readFloat, readSigned)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

parseWorld :: String -> Either String World.World
parseWorld input =
  case parse (spaces *> world) "(unknown)" input of
    Left err -> Left (show err)
    Right w -> Right w

world :: CharParser () World.World
world = do
  openType "World"
  camera <- field "camera" camera
  fieldSep
  viewPlane <- field "viewPlane" viewPlane
  fieldSep
  sceneObjects <- field "sceneObjects" (array sceneObject)
  fieldSep
  lights <- field "lights" (array light)
  fieldSep
  backgroundColor <- field "backgroundColor" color
  closeType
  return World.World
    { World.camera = camera
    , World.viewPlane = viewPlane
    , World.sceneObjects = sceneObjects
    , World.lights = lights
    , World.backgroundColor = backgroundColor
    }

viewPlane :: CharParser () ViewPlane.ViewPlane
viewPlane = do
  openType "ViewPlane"
  hres <- field "horizontalResolution" int
  fieldSep
  vres <- field "verticalResolution" int
  fieldSep
  pixelSize <- field "pixelSize" double
  fieldSep
  sampler <- field "sampler" sampler
  closeType
  return ViewPlane.ViewPlane
    { ViewPlane.horizontalResolution = hres
    , ViewPlane.verticalResolution = vres
    , ViewPlane.pixelSize = pixelSize
    , ViewPlane.pixelSampler = sampler
    }

sampler :: CharParser () Sampler.Sampler
sampler = regular <|> standard

regular :: CharParser () Sampler.Sampler
regular = do
  openType "Regular"
  samplesPerAxis <- field "samplesPerAxis" int
  closeType
  return $ Sampler.mkRegular samplesPerAxis

standard :: CharParser () Sampler.Sampler
standard = do
  string "Standard"
  return Sampler.mkStandard

light :: CharParser () Geometric.Light
light = ambientLight <|> directionalLight <|> pointLight

ambientLight :: CharParser () Geometric.Light
ambientLight = do
  openType "AmbientLight"
  color <- field "color" color
  fieldSep
  ls <- field "ls" double
  closeType
  return $ Light.mkAmbient color ls

directionalLight :: CharParser () Geometric.Light
directionalLight = do
  openType "DirectionalLight"
  invDir <- field "invertDirection" vector3
  fieldSep
  color <- field "color" color
  fieldSep
  ls <- field "ls" double
  closeType
  return $ Light.mkDirectional invDir color ls

pointLight :: CharParser () Geometric.Light
pointLight = do
  openType "PointLight"
  location <- field "location" vector3
  fieldSep
  color <- field "color" color
  fieldSep
  ls <- field "ls" double
  closeType
  return $ Light.mkPoint location color ls

array :: CharParser () a -> CharParser () [a]
array f = char '[' *> spaces *> sepBy f fieldSep <* spaces <* char ']'

sceneObject :: CharParser () World.SceneObject
sceneObject = sphere <|> plane

sphere :: CharParser () World.SceneObject
sphere = do
  openType "Sphere"
  center <- field "center" vector3
  fieldSep
  radius <- field "radius" double
  fieldSep
  material <- field "material" material
  closeType
  return $ World.SceneObject Sphere.Sphere
    { Sphere.center = center
    , Sphere.radius = radius
    , Sphere.material = material
    }

plane :: CharParser () World.SceneObject
plane = do
  openType "Plane"
  point <- field "point" vector3
  fieldSep
  normal <- field "normal" vector3
  fieldSep
  material <- field "material" material
  closeType
  return $ World.SceneObject Plane.Plane
    { Plane.point = point
    , Plane.normal = normal
    , Plane.material = material
    }

material :: CharParser() Geometric.Material
material = matte <|> phong

matte :: CharParser () Geometric.Material
matte = do
  openType "Matte"
  cd <- field "cd" color
  fieldSep
  kd <- field "kd" double
  fieldSep
  ka <- field "ka" double
  closeType
  return $ Material.mkMatte cd kd ka

phong :: CharParser () Geometric.Material
phong = do
  openType "Phong"
  cd <- field "cd" color
  fieldSep
  kd <- field "kd" double
  fieldSep
  ka <- field "ka" double
  fieldSep
  ks <- field "ks" color
  fieldSep
  exp <- field "exp" double
  closeType
  return $ Material.mkPhong cd kd ka ks exp

camera :: CharParser () Camera.Camera
camera = pinholeCamera <|> orthographicCamera

pinholeCamera :: CharParser () Camera.Camera
pinholeCamera = do
  openType "PinholeCamera"
  eye <- field "eye" vector3
  fieldSep
  lookAt <- field "lookAt" vector3
  fieldSep
  up <- field "up" vector3
  fieldSep
  distance <- field "distance" double
  closeType
  return $ Camera.mkPinhole eye lookAt up distance

orthographicCamera :: CharParser () Camera.Camera
orthographicCamera = do
  openType "OrthographicCamera"
  eye <- field "eye" vector3
  fieldSep
  lookAt <- field "lookAt" vector3
  fieldSep
  up <- field "up" vector3
  closeType
  return $ Camera.mkOrthographic eye lookAt up

vector2 :: CharParser () Vector.Vector2
vector2 = do
  string "Vector2"
  Vector.Vector2 <$> (spaces1 *> double) <*> (spaces1 *> double)

vector3 :: CharParser () Vector.Vector3
vector3 = do
  string "Vector3"
  Vector.Vector3 <$> (spaces1 *> double) <*> (spaces1 *> double) <*> (spaces1 *> double)

color :: CharParser () Color.Color
color = do
  string "RGB"
  Color.RGB <$> (spaces1 *> double) <*> (spaces1 *> double) <*> (spaces1 *> double)

openType :: String -> CharParser () ()
openType name = string name *> spaces *> char '{' *> spaces

closeType :: CharParser () ()
closeType = spaces *> char '}' *> spaces

field :: String -> CharParser () a -> CharParser() a
field name valueParser = string name *> spaces *> char '=' *> spaces *> valueParser

fieldSep :: CharParser () ()
fieldSep = spaces *> char ',' *> spaces

double :: CharParser () Double
double = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

int :: CharParser () Int
int = do
  s <- getInput
  case readSigned readDec s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

spaces1 = many1 space

