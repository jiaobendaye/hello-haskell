module Ch03.ListADT(
  List(..)
  , fromList
) where

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList :: [a] -> List a
-- fromList (x:xs) = Cons x (fromList xs)
-- fromList []     = Nil
fromList = foldr Cons Nil 
