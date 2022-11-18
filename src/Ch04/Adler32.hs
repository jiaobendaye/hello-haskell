module Ch04.Adler32(
  adler32
) where

import Data.Char (ord)
import Data.Bits (shiftL, (.&.), (.|.))

base ::Int
base = 65521

adler32 :: [Char] -> Int
adler32 = helper 1 0 
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b []     = (b `shiftL` 16) .|. a