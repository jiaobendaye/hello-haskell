module Ch04.Foldr where

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' step acc (x:xs) = step x (foldr' step acc xs) 
foldr' _ acc []        = acc