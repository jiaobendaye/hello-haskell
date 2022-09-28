module Ch16.FormApp(
  aQuery
) where

import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec

aQuery :: CharParser () [(String, Maybe String)]
aQuery = aPair `sepBy` char '&'

aPair :: CharParser () (String, Maybe String)
aPair =
    liftM2 (,) (many1 aChar) (optionMaybe (char '=' *> many aChar))

urlBaseChars :: [Char]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

aChar :: CharParser () Char
aChar = oneOf urlBaseChars
     <|> (' ' <$ char '+')
     <|> aHex


hexify :: Char -> Char -> Char
hexify a b = toEnum . fst . head .readHex $ [a, b]

aHex :: Parser Char
aHex = hexify <$> (char '%' *> hexDigit) <*> hexDigit