module ARD.Parser
  ( Context(..)
  , parseScene
  , parseWorld
  ) where

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

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Maybe as Maybe
import Numeric (readDec, readFloat, readSigned)
import System.Random
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

data Context
  = Context
  { randomValues :: [Double]
  , camera :: Maybe Camera.Camera
  , viewPlane :: Maybe ViewPlane.ViewPlane
  , sceneObjects :: [World.SceneObject]
  , lights :: [Light.Light]
  , backgroundColor :: Maybe Color.Color
  }

eitherFromMaybe :: Maybe a -> b -> Either b a
eitherFromMaybe Nothing b = Left b
eitherFromMaybe (Just a) _ = Right a

parseWorld :: String -> String -> Either String World.World
parseWorld sourceName input = do
  context <- parseScene sourceName input
  c <- eitherFromMaybe (camera context) "No camera in scene"
  vp <- eitherFromMaybe (viewPlane context) "No view plane in scene"
  return World.World
    { World.camera = c
    , World.viewPlane = vp
    , World.sceneObjects = sceneObjects context
    , World.lights = lights context
    , World.backgroundColor = Maybe.fromMaybe (Color.RGB 0 0 0) (backgroundColor context)
    }

parseScene :: String -> String -> Either String Context
parseScene sourceName input =
  let
    initialContext = Context
      { randomValues = randoms $ mkStdGen 0
      , camera = Nothing
      , viewPlane = Nothing
      , sceneObjects = []
      , lights = []
      , backgroundColor = Nothing
      }
  in
    case runParser pScene initialContext sourceName input of
      Left err -> Left $ show err
      Right context -> Right context

pScene :: CharParser Context Context
pScene = do
  spaces
  manyTill ((pComment <|> pCamera <|> pViewPlane <|> pLight <|> pPlane <|> pSphere <|> pBackgroundColor <|> pRandom) <* spaces) (try eof)
  getState

pBackgroundColor :: CharParser Context ()
pBackgroundColor = do
  try $ string "backgroundColor"
  spaces1
  color <- pColor
  spaces
  updateState $ \c -> c { backgroundColor = Just color }

pRandom :: CharParser Context ()
pRandom = do
  try $ string "random"
  spaces1
  seed <- pInt
  spaces
  updateState $ \c -> c { randomValues = randoms $ mkStdGen seed }

pViewPlane :: CharParser Context ()
pViewPlane = do
  try $ string "viewplane"
  spaces
  char '{'
  spaces
  hres <- pField "horizontal" pInt
  vres <- pField "vertical" pInt
  pixelSize <- pField "pixelsize" pDouble
  sampler <- pField "sampler" pSampler
  spaces
  char '}'
  updateState $ \c -> c { viewPlane = Just ViewPlane.ViewPlane
    { ViewPlane.horizontalResolution = hres
    , ViewPlane.verticalResolution = vres
    , ViewPlane.pixelSize = pixelSize
    , ViewPlane.pixelSampler = sampler
    }
  }

pCamera :: CharParser Context ()
pCamera = do
  try $ string "camera"
  spaces
  char '{'
  spaces
  ctype <- try (pSymbol "orthographic") <|> try (pSymbol "pinhole")
  camera <- case ctype of
    "orthographic" -> do
      eye <- pField "eye" pVector3
      lookAt <- pField "lookAt" pVector3
      up <- pField "up" pVector3
      return $ Camera.mkOrthographic eye lookAt up
    "pinhole" -> do
      eye <- pField "eye" pVector3
      lookAt <- pField "lookAt" pVector3
      up <- pField "up" pVector3
      distance <- pField "distance" pDouble
      return $ Camera.mkPinhole eye lookAt up distance
  spaces
  char '}'
  updateState $ \c -> c { camera = Just camera }

pSphere :: CharParser Context ()
pSphere = do
  try $ string "sphere"
  spaces
  char '{'
  spaces
  center <- pField "center" pVector3
  radius <- pField "radius" pDouble
  material <- pField "material" pMaterial
  spaces
  char '}'
  let
    sphere = World.SceneObject Sphere.Sphere
      { Sphere.center = center
      , Sphere.radius = radius
      , Sphere.material = material
      }
  updateState $ \c -> c { sceneObjects = sceneObjects c ++ [sphere] }

pPlane :: CharParser Context ()
pPlane = do
  try $ string "plane"
  spaces
  char '{'
  spaces
  point <- pField "point" pVector3
  normal <- pField "normal" pVector3
  material <- pField "material" pMaterial
  spaces
  char '}'
  let
    plane = World.SceneObject Plane.Plane
      { Plane.point = point
      , Plane.normal = normal
      , Plane.material = material
      }
  updateState $ \c -> c { sceneObjects = sceneObjects c ++ [plane] }

pMaterial :: CharParser Context Material.Material
pMaterial = do
  char '{'
  spaces
  mtype <- try (pSymbol "matte") <|> try (pSymbol "phong")
  material <- case mtype of
    "matte" -> Material.mkMatte <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble
    "phong" -> Material.mkPhong <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble <*> pField "ks" pDouble <*> pField "exp" pDouble
  spaces
  char '}'
  return material

pLight :: CharParser Context ()
pLight = do
  try $ string "light"
  spaces
  char '{'
  spaces
  ltype <- try (pSymbol "ambient") <|> try (pSymbol "directional") <|> try (pSymbol "point")
  light <- case ltype of
    "ambient" -> Light.mkAmbient <$> pField "color" pColor <*> pField "ls" pDouble
    "directional" -> Light.mkDirectional <$> pField "invertDirection" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
    "point" -> Light.mkPoint <$> pField "location" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
  spaces
  char '}'
  updateState $ \c -> c{ lights = lights c ++ [light] }

pSampler :: CharParser Context Sampler.Sampler
pSampler = do
  char '{'
  spaces
  stype <- try (pSymbol "jittered") <|> try (pSymbol "random") <|> try (pSymbol "regular") <|> try (pSymbol "standard")
  sampler <- case stype of
    "jittered" -> do
      axis <- pField "axis" pInt
      rands <- takeRandomValues (axis*axis*2)
      return $ Sampler.mkJittered axis rands
    "random" -> do
      samples <- pField "samples" pInt
      rands <- takeRandomValues (samples*2)
      return $ Sampler.mkRandom samples rands
    "regular" -> Sampler.mkRegular <$> pField "axis" pInt
    "standard" -> return Sampler.mkStandard
  spaces
  char '}'
  return sampler

pVector2 :: CharParser Context Vector.Vector2
pVector2 = Vector.Vector2 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble)

pVector3 :: CharParser Context Vector.Vector3
pVector3 = Vector.Vector3 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pColor :: CharParser Context Color.Color
pColor = Color.RGB <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pField :: String -> CharParser Context a -> CharParser Context a
pField key valueParser = do
  pSymbol key
  value <- valueParser
  spaces1
  return value

pSymbol :: String -> CharParser Context String
pSymbol symbol = string symbol <* spaces1

pComment :: CharParser Context ()
pComment = do
  char '#'
  manyTill (noneOf "\r\n") (oneOf "\r\n")
  many (oneOf "\r\n")
  return ()

pDouble :: CharParser Context Double
pDouble = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

pInt :: CharParser Context Int
pInt = do
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
  setState context { randomValues = rest }
  return values

