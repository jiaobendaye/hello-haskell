module Ch14.Random(
  rand
  ,twoBadRandoms 
  ,twoGoodRandoms
  ,makeRandomValue
) where

import System.Random
import Ch14.State


rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))


twoBadRandoms :: RandomGen g => g ->(Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

getAny :: (Random a) => State StdGen a
getAny = do
        g <- getSt
        (x, g') <- return $ random g
        putSt g'
        return x

getOne :: (Random a) => (a, a) -> State StdGen a
getOne bounds = do g <- getSt
                   (x, g') <- return $ randomR bounds g
                   putSt g'
                   return x

makeRandomValue :: StdGen -> (Int, StdGen)
makeRandomValue = runState (do  getOne (1, 100))
