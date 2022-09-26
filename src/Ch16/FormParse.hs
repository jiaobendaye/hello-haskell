module Ch16.FormParse where

import Text.ParserCombinators.Parsec
import Numeric
import Control.Monad (liftM2)

pQuery :: CharParser () [(String, Maybe String)]
pQuery = pPair `sepBy` char '&'

pPair :: CharParser () (String, Maybe String)
pPair = do
  name <- many1 pChar
  value <- optionMaybe (char '=' >> many pChar)
  return (name, value)

pPairApp1 :: CharParser () (String, Maybe String)
pPairApp1 =
    liftM2 (,) (many1 pChar) (optionMaybe (char '=' >> many pChar))

pChar :: CharParser () Char
pChar = oneOf urlBaseChars
     <|> (char '+' >> return ' ')
     <|> pHex

urlBaseChars :: [Char]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

pHex :: CharParser () Char
pHex = do
  _ <- char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d