module ARD.ParserSpec where

import ARD.Parser
import qualified ARD.Camera as Camera
import qualified ARD.Vector as Vector

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Test.Hspec

spec :: Spec
spec = describe "Parser" $ do
  it "parses Vector3" $
    p "Vector3 0 1.1 -2" vector3 `shouldBe` Right (Vector.Vector3 0 1.1 (-2))
  it "parses OrthographicCamera" $
    let
      result = p "OrthographicCamera { eye = Vector3 0 0 0,\nlookAt=Vector3 1 1 1, up = Vector3 2 2 2}" camera
    in
      case result of
        Left err   -> expectationFailure $ show err
        Right camera -> Camera.eye camera `shouldBe` Vector.Vector3 0 0 0

p :: String -> CharParser () a -> Either ParseError a
p input f = parse f "(unknown)" input

