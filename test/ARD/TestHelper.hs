{-# OPTIONS_GHC -fno-warn-orphans #-}

module ARD.TestHelper where

import qualified ARD.Color as C
import qualified ARD.Geometric as G
import qualified ARD.Material as Material
import qualified ARD.Ray as Ray
import qualified ARD.Vector as V

import Control.Monad (unless)
import Test.QuickCheck
import Test.Hspec

instance Arbitrary V.Vector2 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ V.Vector2 x y

instance Arbitrary V.Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ V.Vector3 x y z

instance Arbitrary C.Color where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ C.RGB r g b

class TolerantEqual a where
  (=~) :: a -> a -> Bool

-- | Compare equality of Doubles with an epsilon delta.
-- A better approach would be to include the values itself to figure out the epsilon.
-- See https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
instance TolerantEqual Double where
  (=~) x y = abs (x-y) < (1.0e-8 :: Double)

instance TolerantEqual C.Color where
  (=~) (C.RGB r g b) (C.RGB r' g' b') = (r =~ r') && (g =~ g') && (b =~ b')

instance TolerantEqual V.Vector3 where
  (=~) (V.Vector3 x y z) (V.Vector3 x' y' z') = (x =~ x') && (y =~ y') && (z =~ z')

expectTrue :: String -> Bool -> Expectation
expectTrue message actual = unless actual (expectationFailure message)

-- |
-- @actual \`shouldBeClose\` expected@ sets the expectation that @actual@ is equal with some tolerance to @expected@.
shouldBeClose :: (Show a, TolerantEqual a) => a -> a -> Expectation
actual `shouldBeClose` expected = expectTrue ("expected: " ++ show expected ++ "\n but got: " ++ show actual) (actual =~ expected)

hasHitPoint :: (G.GeometricObject a) => a -> Ray.Ray -> V.Point3 -> Expectation
hasHitPoint obj ray hitPoint =
  case G.hit obj ray of
    Just hitResult ->
      let
        shadeRec = G.shadeRecord hitResult
      in
        G.localHitPoint shadeRec `shouldBe` hitPoint
    _ -> expectationFailure "No hit point found"

dummyMaterial :: Material.Material
dummyMaterial = Material.mkMatte (C.RGB 1 1 1) 1 0

