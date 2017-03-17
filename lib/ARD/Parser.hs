module ARD.Parser
  ( Context(..)
  , Globals(..)
  , parseScene
  , parseWorld
  ) where

import qualified ARD.Box as Box
import qualified ARD.Camera as Camera
import qualified ARD.Color as Color
import qualified ARD.Geometric as Geometric
import qualified ARD.Light as Light
import qualified ARD.Material as Material
import qualified ARD.Matrix as Matrix
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
  , ambientLight :: Maybe Light.Light
  , backgroundColor :: Maybe Color.Color
  , globals :: Globals
  }

type SceneParser = CharParser Context

eitherFromMaybe :: Maybe a -> b -> Either b a
eitherFromMaybe Nothing b = Left b
eitherFromMaybe (Just a) _ = Right a

parseWorld :: String -> String -> Either String World.World
parseWorld sourceName input = do
  context <- parseScene sourceName input
  c <- eitherFromMaybe (camera context) "No camera in scene"
  vp <- eitherFromMaybe (viewPlane context) "No view plane in scene"
  ambientLight <- eitherFromMaybe (ambientLight context) "No ambient light in scene"
  return World.World
    { World.camera = c
    , World.viewPlane = vp
    , World.sceneObjects = sceneObjects context
    , World.lights = lights context
    , World.ambientLight = ambientLight
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
      , ambientLight = Nothing
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

pScene :: SceneParser Context
pScene = do
  spaces
  many ((pComment <|> pLet <|> pCamera <|> pViewPlane <|> pLight <|> pPlane <|> pSphere <|> pBox <|> pBackgroundColor <|> pRandom <|> pAmbientLight) <* spaces)
  eof
  getState

pLet :: SceneParser ()
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

pGlobalMaterial :: String -> SceneParser ()
pGlobalMaterial name = do
  try $ pSymbol "material"
  material <- pMaterialBlock
  updateGlobals $ \g -> g { globalMaterials = Map.insert name material (globalMaterials g) }

pGlobalColor :: String -> SceneParser ()
pGlobalColor name = do
  try $ pSymbol "color"
  color <- pColorBlock
  updateGlobals $ \g -> g { globalColors = Map.insert name color (globalColors g) }

pBackgroundColor :: SceneParser ()
pBackgroundColor = do
  try $ pSymbol "backgroundColor"
  color <- pColor
  spaces
  updateState $ \c -> c { backgroundColor = Just color }

pRandom :: SceneParser ()
pRandom = do
  try $ pSymbol "random"
  seed <- pInt
  spaces
  updateState $ \c -> c { randomState = Randomize.mkRandomState seed }

pAmbientLight :: SceneParser ()
pAmbientLight = do
  try $ pSymbol "ambientLight"
  light <- pLightBlock
  spaces
  updateState $ \c -> c { ambientLight = Just light }

pViewPlane :: SceneParser ()
pViewPlane = do
  try $ pSymbol "viewplane"
  pBlock $ do
    hres <- pField "horizontal" pInt
    vres <- pField "vertical" pInt
    pixelSize <- pField "pixelsize" pDouble
    sampler <- pField "sampler" pSampler
    updateState $ \c -> c { viewPlane = Just ViewPlane.ViewPlane
      { ViewPlane.horizontalResolution = hres
      , ViewPlane.verticalResolution = vres
      , ViewPlane.pixelSize = pixelSize
      , ViewPlane.pixelSampler = sampler
      }
    }

pCamera :: SceneParser ()
pCamera = do
  try $ pSymbol "camera"
  camera <- pBlock $ pString >>= \s -> case s of
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
    _ -> fail ("Unexpected camera type " ++ s)
  updateState $ \c -> c { camera = Just camera }

pSphere :: SceneParser ()
pSphere = do
  try $ pSymbol "sphere"
  pBlock $ do
    center <- pField "center" pVector3
    radius <- pField "radius" pDouble
    material <- pField "material" pMaterial
    let
      sphere = World.SceneObject Sphere.Sphere
        { Sphere.center = center
        , Sphere.radius = radius
        , Sphere.material = material
        }
    updateState $ \c -> c { sceneObjects = sceneObjects c ++ [sphere] }

pPlane :: SceneParser ()
pPlane = do
  try $ pSymbol "plane"
  pBlock $ do
    point <- pField "point" pVector3
    normal <- pField "normal" pVector3
    material <- pField "material" pMaterial
    let
      plane = World.SceneObject Plane.Plane
        { Plane.point = point
        , Plane.normal = normal
        , Plane.material = material
        }
    updateState $ \c -> c { sceneObjects = sceneObjects c ++ [plane] }

pBox :: SceneParser ()
pBox = do
  try $ pSymbol "box"
  pBlock $ do
    center <- pField "center" pVector3
    Vector.Vector3 sx sy sz <- pField "size" pVector3
    Vector.Vector3 rx ry rz <- pField "rotation" pVector3
    material <- pField "material" pMaterial
    let
      m = Matrix.rotateX rx `Matrix.mul` Matrix.rotateY ry `Matrix.mul` Matrix.rotateZ rz
      vu = m `Matrix.transformVector` Vector.Vector3 (0.5*sx) 0 0
      vv = m `Matrix.transformVector` Vector.Vector3 0 (0.5*sy) 0
      vw = m `Matrix.transformVector` Vector.Vector3 0 0 (0.5*sz)
      box = World.SceneObject Box.Box
        { Box.center = center
        , Box.vu = vu
        , Box.vv = vv
        , Box.vw = vw
        , Box.material = material
        }
    updateState $ \c -> c { sceneObjects = sceneObjects c ++ [box] }

pMaterial :: SceneParser Material.Material
pMaterial = pReference globalMaterials <|> pMaterialBlock

pMaterialBlock :: SceneParser Material.Material
pMaterialBlock = pBlock $
  pString >>= \s -> case s of
    "matte" -> Material.Matte <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble
    "normal" -> return Material.Normal
    "phong" -> Material.Phong <$> pField "cd" pColor <*> pField "kd" pDouble <*> pField "ka" pDouble <*> pField "ks" pDouble <*> pField "exp" pDouble
    _ -> fail ("Unexpected material type " ++ s)

pLight :: SceneParser ()
pLight = do
  try $ pSymbol "light"
  light <- pLightBlock
  updateState $ \c -> c{ lights = lights c ++ [light] }

pLightBlock :: SceneParser Light.Light
pLightBlock = pBlock $
  pString >>= \s -> case s of
    "ambient" -> Light.Ambient <$> pField "color" pColor <*> pField "ls" pDouble
    "ambientOccluder" -> Light.AmbientOccluder <$> pField "color" pColor <*> pField "ls" pDouble <*> pField "minColor" pColor <*> pField "sampler" pSampler
    "directional" -> Light.Directional <$> pField "invertDirection" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
    "point" -> Light.Point <$> pField "location" pVector3 <*> pField "color" pColor <*> pField "ls" pDouble
    _ -> fail ("Unexpected light type " ++ s)

pSampler :: SceneParser Sampler.Sampler
pSampler = pBlock $
  pString >>= \s -> case s of
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
    _ -> fail ("Unexpected sampler type " ++ s)

pVector2 :: SceneParser Vector.Vector2
pVector2 = Vector.Vector2 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble)

pVector3 :: SceneParser Vector.Vector3
pVector3 = Vector.Vector3 <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pColor :: SceneParser Color.Color
pColor =  pReference globalColors <|> pColorBlock

pColorBlock :: SceneParser Color.Color
pColorBlock = Color.RGB <$> (spaces *> pDouble) <*> (spaces1 *> pDouble) <*> (spaces1 *> pDouble)

pReference :: (Globals -> Map.Map String a) -> SceneParser a
pReference f = do
  name <- try pIdentifier
  context <- getState
  case Map.lookup name $ f $ globals context of
    Just m -> return m
    _ -> fail ("Reference with name '" ++ name ++ "' but global not found")

pField :: String -> SceneParser a -> SceneParser a
pField key valueParser = pSymbol key *> valueParser <* notFollowedBy alphaNum <* spaces

pSymbol :: String -> SceneParser String
pSymbol symbol = string symbol <* notFollowedBy alphaNum <* spaces

pString :: SceneParser String
pString = pIdentifier <* spaces

pIdentifier :: SceneParser String
pIdentifier = do
  first <- letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  return (first:rest)

pComment :: SceneParser ()
pComment = do
  char '#'
  manyTill (noneOf "\r\n") (void (oneOf "\r\n") <|> eof)
  many (oneOf "\r\n")
  return ()

pDouble :: SceneParser Double
pDouble = do
  s <- getInput
  case readSigned readFloat s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

pInt :: SceneParser Int
pInt = do
  s <- getInput
  case readSigned readDec s of
    [(n, s')] -> n <$ setInput s'
    _         -> empty

pBlock :: SceneParser a -> SceneParser a
pBlock = between (char '{' >> spaces) (char '}' >> spaces)

spaces1 :: SceneParser String
spaces1 = many1 space

randomizeFromContext :: Randomize.Randomize a -> SceneParser a
randomizeFromContext f = do
  context <- getState
  let (result, r') = Randomize.runRandomized f (randomState context)
  updateState $ \c -> c { randomState = r' }
  return result

updateGlobals :: (Globals -> Globals) -> SceneParser ()
updateGlobals f = updateState $ \c -> c { globals = f (globals c) }

