module Ch06.Newtype where

-- data DataInt = D Int
--     deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)