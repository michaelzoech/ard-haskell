module ARD.Parser
  ( Context(..)
  , Globals(..)
  , parseScene
  , parseWorld
  ) where

import qualified ARD.Camera as Camera
import qualified ARD.Color as Color
import qualified ARD.Geometric as Geometric
import qualified ARD.Light as Light
import qualified ARD.Material as Material
import qualified ARD.Plane as Plane
import qualified ARD.Randomize as Randomize
import qualified ARD.Sampler as Sampler
import qualified ARD.Sphere as Sphere
import qualified ARD.Vector as Vector
import qualified ARD.ViewPlane as ViewPlane
import qualified ARD.World as World

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Numeric (readDec, readFloat, readSigned)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator
import Text.ParserCombinators.Parsec.Prim

data Globals
  = Globals
  { globalNames :: Set.Set String
  , globalColors :: Map.Map String Color.Color
  , globalMaterials :: Map.Map String Material.Material
  }

data Context
  = Context
  { randomState :: Randomize.Random
  , camera :: Maybe Camera.Camera
  , viewPlane :: Maybe ViewPlane.ViewPlane
  , sceneObjects :: [World.SceneObject]
  , lights :: [Light.Light]
  , backgroundColor :: Maybe Color.Color
  , globals :: Globals
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
    , World.randomState = randomState context
    }

parseScene :: String -> String -> Either String Context
parseScene sourceName input =
  let
    initialContext = Context
      { randomState = Randomize.mkRandomState 0
      , camera = Nothing
      , viewPlane = Nothing
      , sceneObjects = []
      , lights = []
      , backgroundColor = Nothing
      , globals = Globals
        { globalNames = Set.empty
        , globalColors = Map.empty
        , globalMaterials = Map.empty
        }
      }
  in
    case runParser pScene initialContext sourceName input of
      Left err -> Left $ show err
      Right context -> Right context

pScene :: CharParser Context Context
pScene = do
  spaces
  manyTill ((pComment <|> pLet <|> pCamera <|> pViewPlane <|> pLight <|> pPlane <|> pSphere <|> pBackgroundColor <|> pRandom) <* spaces) (try eof)
  getState

pLet :: CharParser Context ()
pLet = do
  try $ pSymbol "let"
  name <- pIdentifier
  context <- getState
  if Set.member name (globalNames $ globals context) then
    fail ("Reference with name '" ++ name ++ "' already declared")
  else do
    updateGlobals $ \g -> g { globalNames = Set.insert name (globalNames g) }
    spaces
    char '='
    spaces
    pGlobalMaterial name <|> pGlobalColor name

pGlobalMaterial :: String -> CharParser Context ()
pGlobalMaterial name = do
  try $ pSymbol "material"
  material <- pMaterialBlock
  updateGlobals $ \g -> g { globalMaterials = Map.insert name material (globalMaterials g) }

pGlobalColor :: String -> CharParser Context ()
pGlobalColor name = do
  try $ pSymbol "color"
  color <- pColorBlock
  updateGlobals $ \g -> g { globalColors = Map.insert name color (globalColors g) }

pBackgroundColor :: CharParser Context ()
pBackgroundColor = do
  try $ pSymbol "backgroundColor"
  color <- pColor
  spaces
  updateState $ \c -> c { backgroundColor = Just color }

pRandom :: CharParser Context ()
pRandom = do
  try $ pSymbol "random"
  seed <- pInt
  spaces
  updateState $ \c -> c { randomState = Randomize.mkRandomState seed }

pViewPlane :: CharParser Context ()
pViewPlane = do
  try $ pSymbol "viewplane"
  pBraceOpen
  hres <- pField "horizontal" pInt
  vres <- pField "vertical" pInt
  pixelSize <- pField "pixelsize" pDouble
  sampler <- pField "sampler" pSampler
  pBraceClose
  updateState $ \c -> c { viewPlane = Just ViewPlane.ViewPlane
    { ViewPlane.horizontalResolution = hres
    , ViewPlane.verticalResolution = vres
    , ViewPlane.pixelSize = pixelSize
    , ViewPlane.pixelSampler = sampler
    }
  }

pCamera :: CharParser Context ()
pCamera = do
  try $ pSymbol "camera"
  pBraceOpen
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
  pBraceClose
  updateState $ \c -> c { camera = Just camera }

pSphere :: CharParser Context ()
pSphere = do
  try $ pSymbol "sphere"
  pBraceOpen
  center <- pField "center" pVector3
  radius <- pField "radius" pDouble
  material <- pField "material" pMaterial
  pBraceClose
  let
    sphere = World.SceneObject Sphere.Sphere
      { Sphere.center = center
      , Sphere.radius = radius
      , Sphere.material = material
      }
  updateState $ \c -> c { sceneObjects = sceneObjects c ++ [sphere] }

pPlane :: CharParser Context ()
pPlane = do
  try $ pSymbol "plane"
  pBraceOpen
  point <- pField "point" pVector3
  normal <- pField "normal" pVector3
  material <- pField "material" pMaterial
  pBraceClose
  let
    plane = World.SceneObject Plane.Plane
      { Plane.point = point
      , Plane.normal = normal
      , Plane.material = material
      }
  updateState $ \c -> c { sceneObjects = sceneObjects c ++ [plane] }

pMaterial :: CharParser Context Material.Material
pMaterial = pReference globalMaterials <|> pMaterialBlock

pMaterialBlock :: CharParser Context Material.Material
pMaterialBlock = do
  try pBraceOpen
  mtype <- try (pSymbol "matte") <|> try (pSymbol "phong")
  material <- case mtype of
    "matte" -> Material.mkMatte <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble
    "phong" -> Material.mkPhong <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble <*> pField "ks" pDouble <*> pField "exp" pDouble
  pBraceClose
  return material

pLight :: CharParser Context ()
pLight = do
  try $ pSymbol "light"
  pBraceOpen
  ltype <- try (pSymbol "ambient") <|> try (pSymbol "directional") <|> try (pSymbol "point")
  light <- case ltype of
    "ambient" -> Light.mkAmbient <$> pField "color" pColor <*> pField "ls" pDouble
    "directional" -> Light.mkDirectional <$> pField "invertDirection" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
    "point" -> Light.mkPoint <$> pField "location" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
  pBraceClose
  updateState $ \c -> c{ lights = lights c ++ [light] }

pSampler :: CharParser Context Sampler.Sampler
pSampler = do
  pBraceOpen
  stype <- try (pSymbol "jittered") <|> try (pSymbol "random") <|> try (pSymbol "regular") <|> try (pSymbol "standard")
  sampler <- case stype of
    "jittered" -> do
      axis <- pField "axis" pInt
      randomizeFromContext (Sampler.mkJittered axis)
    "random" -> do
      samples <- pField "samples" pInt
      randomizeFromContext (Sampler.mkRandom samples)
    "regular" -> do
      axis <- pField "axis" pInt
      randomizeFromContext (Sampler.mkRegular axis)
    "standard" -> randomizeFromContext Sampler.mkStandard
  pBraceClose
  return sampler

pVector2 :: CharParser Context Vector.Vector2
pVector2 = Vector.Vector2 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble)

pVector3 :: CharParser Context Vector.Vector3
pVector3 = Vector.Vector3 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pColor :: CharParser Context Color.Color
pColor =  pReference globalColors <|> pColorBlock

pColorBlock :: CharParser Context Color.Color
pColorBlock = Color.RGB <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pReference :: (Globals -> Map.Map String a) -> CharParser Context a
pReference f = do
  name <- try pIdentifier
  context <- getState
  case Map.lookup name $ f $ globals context of
    Just m -> return m
    _ -> fail ("Reference with name '" ++ name ++ "' but global not found")

pField :: String -> CharParser Context a -> CharParser Context a
pField key valueParser = pSymbol key *> valueParser <* notFollowedBy alphaNum <* spaces

pSymbol :: String -> CharParser Context String
pSymbol symbol = string symbol <* notFollowedBy alphaNum <* spaces

pIdentifier :: CharParser Context String
pIdentifier = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (first:rest)

pComment :: CharParser Context ()
pComment = do
  char '#'
  manyTill (noneOf "\r\n") (void (oneOf "\r\n") <|> eof)
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

pBraceOpen :: CharParser Context ()
pBraceOpen = char '{' >> spaces

pBraceClose :: CharParser Context ()
pBraceClose = char '}' >> spaces

spaces1 :: CharParser Context String
spaces1 = many1 space

randomizeFromContext :: Randomize.Randomize a -> CharParser Context a
randomizeFromContext f = do
  context <- getState
  let (result, r') = Randomize.runRandomized f (randomState context)
  updateState $ \c -> c { randomState = r' }
  return result

updateGlobals :: (Globals -> Globals) -> CharParser Context ()
updateGlobals f = updateState $ \c -> c { globals = f (globals c) }

