module Ch04.Foldl where

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' step acc (x:xs) = foldl' step (step acc x) xs
foldl' _ acc []        = acc


myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)