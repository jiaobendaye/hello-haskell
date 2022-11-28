module Ch11.Prettify2 where

import Test.QuickCheck
import Data.Char
import Control.Monad
import Prelude hiding ((<>))

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         deriving (Show,Eq)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]


empty :: Doc
empty = Empty

(<>)  :: Doc -> Doc -> Doc
(<>) = undefined
