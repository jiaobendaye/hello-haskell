module Ch15.RandomSupply (
  randomsIO
) where


import Ch15.Supply
import System.Random hiding (next)

randomsIO :: Random a => IO [a]
randomsIO =
        getStdRandom $ \g ->
                let (a, b) = split g
                in (randoms a, b)