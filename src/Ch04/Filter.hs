module Ch04.Filter where

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr step []
    where step x ys | p x       = x : ys
                    | otherwise = ys