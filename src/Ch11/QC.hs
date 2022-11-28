module Ch11.QC where

import Ch05.Prettify
import Ch05.SimpleJSON
import Data.List
import Prelude hiding((<>))

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

prop_char c   = char c   == Char c

prop_text s   = text s   == if null s then Empty else Text s

prop_line     = line     == Line

prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
    where
        glue []     = empty
        glue (d:ds) = d <> glue ds

prop_empty_id x = empty <> x == x && x <> empty == x

prop_punctuate s xs = punctuate s xs == intersperse s xs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
    where
        combine []           = []
        combine [x]          = [x]

        combine (x:Empty:ys) = x : combine ys
        combine (Empty:y:ys) = y : combine ys
        combine (x:y:ys)     = x `Concat` y : combine ys