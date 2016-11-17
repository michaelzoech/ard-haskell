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
import System.Random
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

data Context
  = Context
  { randomValues :: [Double]
  }

parseWorld :: String -> String -> Either String World.World
parseWorld sourceName input =
  let
    values = (randoms $ mkStdGen 0) :: [Double]
    context = Context { randomValues = values }
  in
    case runParser (spaces *> world) context sourceName input of
      Left err -> Left (show err)
      Right w -> Right w

world :: CharParser Context World.World
world = do
  openBrace "World"
  camera <- field "camera" camera
  viewPlane <- field' "viewPlane" viewPlane
  sceneObjects <- field' "sceneObjects" (array sceneObject)
  lights <- field' "lights" (array light)
  backgroundColor <- field' "backgroundColor" color
  closeBrace
  return World.World
    { World.camera = camera
    , World.viewPlane = viewPlane
    , World.sceneObjects = sceneObjects
    , World.lights = lights
    , World.backgroundColor = backgroundColor
    }

viewPlane :: CharParser Context ViewPlane.ViewPlane
viewPlane = do
  openBrace "ViewPlane"
  hres <- field "horizontalResolution" int
  vres <- field' "verticalResolution" int
  pixelSize <- field' "pixelSize" double
  sampler <- field' "sampler" sampler
  closeBrace
  return ViewPlane.ViewPlane
    { ViewPlane.horizontalResolution = hres
    , ViewPlane.verticalResolution = vres
    , ViewPlane.pixelSize = pixelSize
    , ViewPlane.pixelSampler = sampler
    }

sampler :: CharParser Context Sampler.Sampler
sampler = jitteredSampler <|> randomSampler <|> regularSampler <|> standardSampler

jitteredSampler :: CharParser Context Sampler.Sampler
jitteredSampler = do
  openBrace "Jittered"
  samplesPerAxis <- field "samplesPerAxis" int
  closeBrace
  rands <- takeRandomValues (samplesPerAxis*samplesPerAxis*2)
  return $ Sampler.mkJittered samplesPerAxis rands

randomSampler :: CharParser Context Sampler.Sampler
randomSampler = do
  openBrace "Random"
  numSamples <- field "numSamples" int
  closeBrace
  rands <- takeRandomValues (numSamples*2)
  return $ Sampler.mkRandom numSamples rands

regularSampler :: CharParser Context Sampler.Sampler
regularSampler = do
  openBrace "Regular"
  samplesPerAxis <- field "samplesPerAxis" int
  closeBrace
  return $ Sampler.mkRegular samplesPerAxis

standardSampler :: CharParser Context Sampler.Sampler
standardSampler = do
  string "Standard"
  return Sampler.mkStandard

light :: CharParser Context Light.Light
light = ambientLight <|> directionalLight <|> pointLight

ambientLight :: CharParser Context Light.Light
ambientLight = do
  openBrace "AmbientLight"
  color <- field "color" color
  ls <- field' "ls" double
  closeBrace
  return $ Light.mkAmbient color ls

directionalLight :: CharParser Context Light.Light
directionalLight = do
  openBrace "DirectionalLight"
  invDir <- field "invertDirection" vector3
  color <- field' "color" color
  ls <- field' "ls" double
  closeBrace
  return $ Light.mkDirectional invDir color ls

pointLight :: CharParser Context Light.Light
pointLight = do
  openBrace "PointLight"
  location <- field "location" vector3
  color <- field' "color" color
  ls <- field' "ls" double
  closeBrace
  return $ Light.mkPoint location color ls

sceneObject :: CharParser Context World.SceneObject
sceneObject = sphere <|> plane

sphere :: CharParser Context World.SceneObject
sphere = do
  openBrace "Sphere"
  center <- field "center" vector3
  radius <- field' "radius" double
  material <- field' "material" material
  closeBrace
  return $ World.SceneObject Sphere.Sphere
    { Sphere.center = center
    , Sphere.radius = radius
    , Sphere.material = material
    }

plane :: CharParser Context World.SceneObject
plane = do
  openBrace "Plane"
  point <- field "point" vector3
  normal <- field' "normal" vector3
  material <- field' "material" material
  closeBrace
  return $ World.SceneObject Plane.Plane
    { Plane.point = point
    , Plane.normal = normal
    , Plane.material = material
    }

material :: CharParser Context Material.Material
material = matte <|> phong

matte :: CharParser Context Material.Material
matte = do
  openBrace "Matte"
  cd <- field "cd" color
  kd <- field' "kd" double
  ka <- field' "ka" double
  closeBrace
  return $ Material.mkMatte cd kd ka

phong :: CharParser Context Material.Material
phong = do
  openBrace "Phong"
  cd <- field "cd" color
  kd <- field' "kd" double
  ka <- field' "ka" double
  ks <- field' "ks" double
  exp <- field' "exp" double
  closeBrace
  return $ Material.mkPhong cd kd ka ks exp

camera :: CharParser Context Camera.Camera
camera = pinholeCamera <|> orthographicCamera

pinholeCamera :: CharParser Context Camera.Camera
pinholeCamera = do
  openBrace "PinholeCamera"
  eye <- field "eye" vector3
  lookAt <- field' "lookAt" vector3
  up <- field' "up" vector3
  distance <- field' "distance" double
  closeBrace
  return $ Camera.mkPinhole eye lookAt up distance

orthographicCamera :: CharParser Context Camera.Camera
orthographicCamera = do
  openBrace "OrthographicCamera"
  eye <- field "eye" vector3
  lookAt <- field' "lookAt" vector3
  up <- field' "up" vector3
  closeBrace
  return $ Camera.mkOrthographic eye lookAt up

vector2 :: CharParser Context Vector.Vector2
vector2 = do
  string "Vector2"
  Vector.Vector2 <$> (spaces1 *> double) <*> (spaces1 *> double)

vector3 :: CharParser Context Vector.Vector3
vector3 = do
  string "Vector3"
  Vector.Vector3 <$> (spaces1 *> double) <*> (spaces1 *> double) <*> (spaces1 *> double)

color :: CharParser Context Color.Color
color = do
  string "RGB"
  Color.RGB <$> (spaces1 *> double) <*> (spaces1 *> double) <*> (spaces1 *> double)

openBrace :: String -> CharParser Context ()
openBrace name = try (string name) *> spaces *> char '{' *> spaces

closeBrace :: CharParser Context ()
closeBrace = spaces *> char '}' *> spaces

array :: CharParser Context a -> CharParser Context [a]
array f = char '[' *> spaces *> sepBy f fieldSep <* spaces <* char ']'

field' :: String -> CharParser Context a -> CharParser Context a
field' name valueParser = fieldSep *> field name valueParser

field :: String -> CharParser Context a -> CharParser Context a
field name valueParser = string name *> spaces *> char '=' *> spaces *> valueParser

fieldSep :: CharParser Context ()
fieldSep = spaces *> char ',' *> spaces

double :: CharParser Context Double
double = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

int :: CharParser Context Int
int = do
  s <- getInput
  case readSigned readDec s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

spaces1 :: CharParser Context String
spaces1 = many1 space

takeRandomValues :: Int -> CharParser Context [Double]
takeRandomValues n = do
  context <- getState
  let (values, rest) = splitAt n (randomValues context)
  setState Context { randomValues = rest }
  return values

