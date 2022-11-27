module Ch11.QC where

import Ch11.Prettify2

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty
