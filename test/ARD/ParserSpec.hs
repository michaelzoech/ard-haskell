module ARD.ParserSpec where

import qualified ARD.Parser as Parser
import qualified ARD.Camera as Camera
import qualified ARD.Ray as Ray
import qualified ARD.Vector as V

import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import Test.Hspec
import Text.ParserCombinators.Parsec

spec :: Spec
spec = describe "Parser" $ do
  it "parses empty scene" $
    Either.isRight (Parser.parseScene "" "") `shouldBe` True
  it "parses comments" $
    Either.isRight (Parser.parseScene "" "#c1\n#c2\n") `shouldBe` True
  it "parses perspective camera" $ do
    let
      scene =
        "camera { \
          \pinhole \
          \eye 0 1 2 \
          \lookAt 1 2 3 \
          \up 2 3 4 \
          \distance 1 \
        \}"
    context <- pSuccess scene
    let
      camera = Parser.camera context
    Maybe.isJust camera `shouldBe` True

pSuccess :: String -> IO Parser.Context
pSuccess input =
  case Parser.parseScene "" input of
    (Left err) -> fail ("Expected success but failed with:\n" ++ show err)
    (Right context) -> return context

{-
  it "Vector3" $
    p Parser.vector3 "Vector3 0 1.1 -2" `shouldBe` Right (V.Vector3 0 1.1 (-2))
  it "OrthographicCamera" $ do
    actual <- psuccess Parser.camera "OrthographicCamera { eye = Vector3 0 0 1, lookAt=Vector3 0 0 0, up = Vector3 0 1 0}"
    Camera.eye actual `shouldBe` V.Vector3 0 0 1
    Camera.lookAt actual `shouldBe` V.Vector3 0 0 0
    Camera.up actual `shouldBe` V.Vector3 0 1 0
    Camera.generateRay actual (V.Vector2 1 1) `shouldBe` Ray.Ray { Ray.origin = V.Vector3 1 1 1, Ray.direction = V.Vector3 0 0 (-1) }


perror :: CharParser Parser.Context b -> String -> IO ()
perror f input =
  case parse f "" input of
    (Left _) -> return ()
    (Right _) -> fail "Expected a parse error but parsed successfully"

p :: CharParser Parser.Context a -> String -> Either ParseError a
p f = parse f ""
-}
