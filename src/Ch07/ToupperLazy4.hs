module Ch07.ToupperLazy4 where

import Data.Char(toUpper)

main :: IO ()
main = interact (map toUpper)