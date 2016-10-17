module ARD.SamplerSpec where

import ARD.Sampler
import ARD.Vector2
import Test.Hspec

spec :: Spec
spec = describe "Sampler" $ do
  standardSamplerSpec
  regularSamplerSpec

standardSamplerSpec :: Spec
standardSamplerSpec = describe "StandardSampler" $ do
  it "has 1 point in the center" $
    genStandardSampler `shouldBe` (Sampler 1 [Vector2 0.5 0.5])

regularSamplerSpec :: Spec
regularSamplerSpec = describe "RegularSampler" $ do
  it "distributes evenly" $
    genRegularSampler 4 `shouldBe` (Sampler 4 [Vector2 0.25 0.25, Vector2 0.75 0.25, Vector2 0.25 0.75, Vector2 0.75 0.75])

