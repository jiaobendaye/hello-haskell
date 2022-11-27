module Ch11.Arbitrary where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- class Arbitrary a where
--     arbitrary   :: Gen a
--     elements :: [a] -> Gen a
--     choose   :: Random a => (a, a) -> Gen a
--     oneof    :: [Gen a] -> Gen a

data Ternary
    = Yes
    | No
    | Unknown
    deriving (Eq,Show)

instance Arbitrary Ternary where
    arbitrary     = 
      oneof [
        return Yes,
        return No,
        return Unknown
      ]
