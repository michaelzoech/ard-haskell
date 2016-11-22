module ARD.SamplerSpec where

import ARD.Randomize
import ARD.Sampler
import ARD.Vector

import Test.Hspec

spec :: Spec
spec = describe "Sampler" $ do
  jitteredSamplerSpec
  standardSamplerSpec
  randomSamplerSpec
  regularSamplerSpec

randomSampler :: [Double] -> Randomize Sampler -> Sampler
randomSampler rs samplerFunc = fst $ runRandomized samplerFunc (mkRandomState' [] rs)

jitteredSamplerSpec :: Spec
jitteredSamplerSpec = describe "JitteredSampler" $ do
  it "with non-random values matches RegularSampler" $
    randomSampler [0.5,0.5..] (mkJittered 2) `shouldBe` mkRegular 2
  it "uses given list to jitter points" $ do
    unitSquareSamples (randomSampler [0,0..] (mkJittered 2)) `shouldBe` [Vector2 0 0, Vector2 0.5 0, Vector2 0 0.5, Vector2 0.5 0.5]
    unitSquareSamples (randomSampler [1,1..] (mkJittered 2)) `shouldBe` [Vector2 0.5 0.5, Vector2 1 0.5, Vector2 0.5 1, Vector2 1 1]

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
    numSamples (randomSampler [0..] (mkRandom 2)) `shouldBe` 2
    numSamples (randomSampler [0..] (mkRandom 3)) `shouldBe` 3
  it "uses given list to generate samples" $
    unitSquareSamples (randomSampler [0..4] (mkRandom 2)) `shouldNotBe` unitSquareSamples (randomSampler [1..5] (mkRandom 2))

regularSamplerSpec :: Spec
regularSamplerSpec = describe "RegularSampler" $ do
  it "has squared number of given argument" $ do
    numSamples (mkRegular 2) `shouldBe` 4
    numSamples (mkRegular 3) `shouldBe` 9
  it "distributes evenly" $
    unitSquareSamples (mkRegular 2) `shouldBe` [Vector2 0.25 0.25, Vector2 0.75 0.25, Vector2 0.25 0.75, Vector2 0.75 0.75]

