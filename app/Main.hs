module Main (main) where

import Lib
import Ch14.Random
import Ch14.Random
import Ch15.HandleIO
import Ch15.SafeHello
import Ch15.Supply 
import Ch15.TidyHello
import Ch15.WriterIO
import System.Random hiding (next)

main :: IO ()
main =  do
  putStrLn "test begain"
  -- testSafeHello
  -- testWriteIO
  testTidyHello
  putStrLn "test done"

testSafeHello:: IO()
testSafeHello= do
  safeHello' "abc.txt"

testWriteIO :: IO()
testWriteIO = do
  let a = runWriterIO (safeHello' "foo")
  print a

testTidyHello :: IO()
testTidyHello = do
  tidyHello' "abc.txt"

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
