module ARD.ParserSpec where

import qualified ARD.Parser as Parser
import qualified ARD.Camera as Camera
import qualified ARD.Ray as Ray
import qualified ARD.Vector as V

import Test.Hspec
import Text.ParserCombinators.Parsec

spec :: Spec
spec = describe "Parser" $ do
  it "Vector3" $
    p Parser.vector3 "Vector3 0 1.1 -2" `shouldBe` Right (V.Vector3 0 1.1 (-2))
  it "OrthographicCamera" $ do
    actual <- psuccess Parser.camera "OrthographicCamera { eye = Vector3 0 0 1, lookAt=Vector3 0 0 0, up = Vector3 0 1 0}"
    Camera.eye actual `shouldBe` V.Vector3 0 0 1
    Camera.lookAt actual `shouldBe` V.Vector3 0 0 0
    Camera.up actual `shouldBe` V.Vector3 0 1 0
    Camera.generateRay actual (V.Vector2 1 1) `shouldBe` Ray.Ray { Ray.origin = V.Vector3 1 1 1, Ray.direction = V.Vector3 0 0 (-1) }

psuccess :: CharParser () b -> String -> IO b
psuccess f input =
  case parse f "" input of
    (Left err) -> fail ("Expected success but failed with:\n" ++ show err)
    (Right actual) -> return actual

perror :: CharParser () b -> String -> IO ()
perror f input =
  case parse f "" input of
    (Left _) -> return ()
    (Right _) -> fail "Expected a parse error but parsed successfully"

p :: CharParser () a -> String -> Either ParseError a
p f = parse f ""

