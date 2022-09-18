module Main (main) where

import Lib
import Ch14.Random
import Ch15.Supply 
import System.Random hiding (next)


main :: IO ()
main = do
  g <- getStdGen
  let b = twoBadRandoms g
  print b
  let a = runSupply next [1,2,3]
  print a