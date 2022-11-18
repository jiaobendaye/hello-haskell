module Ch04.Suffixes where

import Data.List (tails)

suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes [] = []

noAsPattern :: [a] -> [[a]]
noAsPattern (x:xs) = (x:xs) : noAsPattern xs
noAsPattern [] = []

suffixes2 :: [a] -> [[a]]
suffixes2 xs = init (tails xs)