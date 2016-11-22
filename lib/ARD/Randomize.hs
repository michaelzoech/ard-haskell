module ARD.Randomize
  ( mkRandomState
  , mkRandomState'
  , Random
  , Randomize
  , RandomValue(..)
  , runRandomized
  ) where

import Control.Monad
import Control.Monad.State
import qualified System.Random as R

data Random
  = Random
  { randomDoubles :: [Double]
  , randomInts :: [Int]
  }

type Randomize a = State Random a

class RandomValue a where
  getRandom :: Randomize a
  getRandom = do
    r <- getRandoms 1
    return (head r)
  getRandoms :: Int -> Randomize [a]

instance RandomValue Double where
  getRandoms n = do
    state <- get
    let
      (r,rs) = splitAt n $ randomDoubles state
    put $ state { randomDoubles = rs }
    return r

instance RandomValue Int where
  getRandoms n = do
    state <- get
    let
      (r,rs) = splitAt n $ randomInts state
    put $ state { randomInts = rs }
    return r

mkRandomState :: Int -> Random
mkRandomState r =
  let
    gen = R.mkStdGen r
  in
    Random
      { randomDoubles = R.randoms gen
      , randomInts = R.randoms gen
      }

mkRandomState' :: [Int] -> [Double] -> Random
mkRandomState' ints doubles =
  Random
    { randomDoubles = doubles
    , randomInts = ints
    }

runRandomized :: Randomize a -> Random -> (a, Random)
runRandomized = runState

