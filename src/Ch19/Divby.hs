module Ch19.Divby(
  divBy1
  ,divBy2
  ,divBym1
  ,divBym2
  ,divBy3
  ,divBy6
  ,divBy7
  ,DivByError(..)
  -- ,divByGeneric
  -- ,divBy5
) where


divBy1 :: Integral a => a -> [a] -> [a]
divBy1 numerator = map (numerator `div`)

divBy2 :: Integral a => a -> [a] -> Maybe [a]
divBy2 _ [] = Just []
divBy2 _ (0:_) = Nothing
divBy2 numerator (denom:xs) =
    case divBy2 numerator xs of
      Nothing -> Nothing
      Just results -> Just ((numerator `div` denom) : results)

divBym1 :: Integral a => a -> [a] -> Maybe [a]
divBym1 numerator = 
    mapM (numerator `safeDiv`) 
    where safeDiv _ 0 = Nothing
          safeDiv x y = Just (x `div` y)

divBym2 :: Integral a => a -> [a] -> Maybe [a]
divBym2 _ [] = return []
divBym2 _ (0:_) = fail "division by zero in divBy"
divBym2 numerator (denom:xs) =
    do next <- divBym2 numerator xs
       return ((numerator `div` denom) : next)
        
divBy3 :: Integral a => a -> [a] -> [Maybe a]
divBy3 numerator =
    map worker 
    where worker 0 = Nothing
          worker x = Just (numerator `div` x)

-- divBy5 :: Integral a => a -> [a] -> Maybe [a]
-- divBy5 = divByGeneric

-- divByGeneric :: (Monad m, Integral a) => a -> [a] -> m [a]
-- divByGeneric _ [] = return []
-- divByGeneric _ (0:_) = fail "division by zero in divByGeneric"
-- divByGeneric numerator (denom:xs) =
--     do next <- divByGeneric numerator xs
--        return ((numerator `div` denom) : next)

divBy6 :: Integral a => a -> [a] -> Either String [a]
divBy6 _ [] = Right []
divBy6 _ (0:_) = Left "divBy: division by 0"
divBy6 numerator (denom:xs) =
    case divBy6 numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)

data DivByError a = DivBy0
                 | ForbiddenDenominator a
                   deriving (Eq, Read, Show)

divBy7 :: Integral a => a -> [a] -> Either (DivByError a) [a]
divBy7 _ [] = Right []
divBy7 _ (0:_) = Left DivBy0
divBy7 _ (10:_) = Left (ForbiddenDenominator 10)
divBy7 _ (20:_) = Left (ForbiddenDenominator 20)
divBy7 numerator (denom:xs) =
    case divBy7 numerator xs of
      Left x -> Left x
      Right results -> Right ((numerator `div` denom) : results)