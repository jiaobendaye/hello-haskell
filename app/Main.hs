module Main (main) where

import Lib
import Ch14.Random
import qualified Ch15.Supply as S
import System.Random


main :: IO ()
main = do
  g <- getStdGen
  let b = twoBadRandoms g
  print b
  let a = S.runSupply S.next [1,2,3]
  print a