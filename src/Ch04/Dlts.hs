module Ch04.Dlts(
  dlts
  , dlts2
) where

import Data.List (isPrefixOf)

-- #define DLT_EN10MB      1       /* Ethernet (10Mb) */
-- #define DLT_EN3MB       2       /* Experimental Ethernet (3Mb) */
-- #define DLT_AX25        3       /* Amateur Radio AX.25 */

dlts :: String -> [String]
dlts = foldr step [] . lines
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise                     = ds
        secondWord = head . tail . words

dlts2 :: String -> [String]
dlts2 = map (head . tail . words) . filter ("#define DLT_" `isPrefixOf`) . lines