module ARD.SamplerSpec where

import ARD.Sampler
import ARD.Vector

import Test.Hspec

spec :: Spec
spec = describe "Sampler" $ do
  jitteredSamplerSpec
  standardSamplerSpec
  randomSamplerSpec
  regularSamplerSpec

jitteredSamplerSpec :: Spec
jitteredSamplerSpec = describe "JitteredSampler" $ do
  it "with non-random values matches RegularSampler" $
    mkJittered 2 [0.5,0.5..] `shouldBe` mkRegular 2
  it "uses given list to jitter points" $ do
    unitSquareSamples (mkJittered 2 [0,0..]) `shouldBe` [Vector2 0 0, Vector2 0.5 0, Vector2 0 0.5, Vector2 0.5 0.5]
    unitSquareSamples (mkJittered 2 [1,1..]) `shouldBe` [Vector2 0.5 0.5, Vector2 1 0.5, Vector2 0.5 1, Vector2 1 1]

standardSamplerSpec :: Spec
standardSamplerSpec = describe "StandardSampler" $
  it "has 1 point in the center" $ do
    let
      s = mkStandard
    numSamples s `shouldBe` 1
    unitSquareSamples s `shouldBe` [Vector2 0.5 0.5]

randomSamplerSpec :: Spec
randomSamplerSpec = describe "RandomSampler" $ do
  it "has given number of samples" $ do
    numSamples (mkRandom 2 [0..]) `shouldBe` 2
    numSamples (mkRandom 3 [0..]) `shouldBe` 3
  it "uses given list to generate samples" $
    unitSquareSamples (mkRandom 2 [0..4]) `shouldNotBe` unitSquareSamples (mkRandom 2 [1..5])

regularSamplerSpec :: Spec
regularSamplerSpec = describe "RegularSampler" $ do
  it "has squared number of given argument" $ do
    numSamples (mkRegular 2) `shouldBe` 4
    numSamples (mkRegular 3) `shouldBe` 9
  it "distributes evenly" $
    unitSquareSamples (mkRegular 2) `shouldBe` [Vector2 0.25 0.25, Vector2 0.75 0.25, Vector2 0.25 0.75, Vector2 0.75 0.75]

