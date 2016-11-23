module ARD.Randomize
  ( mkRandomState
  , mkRandomState'
  , Random
  , Randomize
  , RandomValue(..)
  , runRandomized
  , shuffle
  ) where

import Control.Monad
import Control.Monad.State
import qualified Data.List as List
import qualified System.Random as R

data Random
  = Random
  { randomDoubles :: [Double]
  , randomInts :: [Int]
  }

type Randomize a = State Random a

class RandomValue a where
  getRandoms :: Int -> Randomize [a]
  getRandom :: Randomize a
  getRandom = do
    r <- getRandoms 1
    return (head r)

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

shuffle :: [a] -> Randomize [a]
shuffle xs = do
  list <- getRandoms (length xs) :: Randomize [Int]
  return $ map snd $ List.sortOn fst (zip list xs)

