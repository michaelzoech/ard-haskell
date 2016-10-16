{-# OPTIONS_GHC -fno-warn-orphans #-}

module ARD.TestHelper where

import qualified ARD.Color as Color
import qualified ARD.Geometric as Geometric
import qualified ARD.Plane as Plane
import qualified ARD.Ray as Ray
import qualified ARD.Vector3 as Vector3
import Control.Monad (unless)
import Test.QuickCheck
import Test.Hspec

instance Arbitrary Vector3.Vector3 where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Vector3.Vector3 x y z

instance Arbitrary Color.Color where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ Color.RGB r g b

class TolerantEqual a where
  (=~) :: a -> a -> Bool

-- | Compare equality of Doubles with an epsilon delta.
-- A better approach would be to include the values itself to figure out the epsilon.
-- See https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/
instance TolerantEqual Double where
  (=~) x y = abs (x-y) < (1.0e-8 :: Double)

instance TolerantEqual Color.Color where
  (=~) (Color.RGB r g b) (Color.RGB r' g' b') = (r =~ r') && (g =~ g') && (b =~ b')

expectTrue :: String -> Bool -> Expectation
expectTrue message actual = unless actual (expectationFailure message)

-- |
-- @actual \`shouldBeClose\` expected@ sets the expectation that @actual@ is equal with some tolerance to @expected@.
shouldBeClose :: (Show a, TolerantEqual a) => a -> a -> Expectation
actual `shouldBeClose` expected = expectTrue ("expected: " ++ show expected ++ "\n but got: " ++ show actual) (actual =~ expected)

hasHitPoint :: (Geometric.GeometricObject a) => a -> Ray.Ray -> Vector3.Point3 -> Expectation
hasHitPoint obj ray hitPoint =
  case Geometric.hit obj ray of
    Just hitResult ->
      let shadeRec = Geometric.shadeRecord hitResult
      in (Geometric.localHitPoint shadeRec) `shouldBe` hitPoint
    _ -> expectationFailure "No hit point found"

