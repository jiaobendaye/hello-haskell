module Main (main) where

import Lib
import Ch14.Random
import Ch15.Supply 
import Ch15.HandleIO
import Ch15.SafeHello
import System.Random hiding (next)


main :: IO ()
main = testMonadHandleIO

testRandom:: IO()
testRandom  = do
  g <- getStdGen
  let b = twoBadRandoms g
  print b
  let a = runSupply next [1,2,3]
  print a


testHandleIO :: IO()
testHandleIO = do
  putStrLn "out path"
  path <- getLine
  runHandleIO (safeHello path)
  runHandleIO (tidyHello path)

testMonadHandleIO :: IO()
testMonadHandleIO = do
  safeHello' "abc.txt"