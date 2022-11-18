module Ch04.StrictFoldl where

foldl' :: (t -> a -> t) -> t -> [a] -> t
foldl' step acc (x:xs) = 
    let new = step acc x
    in new `seq` foldl' step new xs
foldl' _ acc [] = acc