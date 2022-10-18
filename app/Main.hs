module Main (main) where

import Lib
import Ch14.Random
import Ch14.Random
import Ch15.HandleIO
import Ch15.SafeHello
import Ch15.Supply 
import Ch15.TidyHello
import Ch15.WriterIO
import Ch18.CountEntriesT (countEntries)
import Text.ParserCombinators.Parsec (parseTest)
import Control.Monad.Writer (execWriterT)
import Ch16.FormApp
import Ch16.FormParse
import System.Random hiding (next)


main :: IO ()
main =  do
  putStrLn "test begain"
  -- testSafeHello
  -- testWriteIO
  -- testTidyHello
  -- testFormParse
  -- testFormParseApp
  testCountEntries
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

testFormParse :: IO()
testFormParse =  do
  parseTest pQuery "foo=bar&a%21=b+c"

testFormParseApp :: IO()
testFormParseApp =  do
  parseTest aQuery "foo=bar&a%21=b+c"

testCountEntries :: IO()
testCountEntries = do
  a <-  execWriterT $ countEntries "."
  print $ take 4 a