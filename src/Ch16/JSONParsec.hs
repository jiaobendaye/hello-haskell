{-# LANGUAGE FlexibleContexts #-}

module Ch16.JSONParsec where

import Control.Monad (mzero)
import Ch16.JSONClass
import Numeric
import Text.ParserCombinators.Parsec

pText :: CharParser () JValue
pText = spaces *> text
     <?> "JSON text"
    where text = JObject <$> pObject
             <|> JArray <$> pArray

pArray :: CharParser () (JAry JValue)
pArray = JAry <$> pSeries '[' pValue ']'

pSeries :: Char -> CharParser () a -> Char -> CharParser () [a]
pSeries left parser right =
    between (char left <* spaces) (char right) $
            (parser <* spaces) `sepBy` (char ',' <* spaces)

pObject :: CharParser () (JObj JValue)
pObject = JObj <$> pSeries '{' pField '}'
    where pField = (,) <$> (pString <* char ':' <* spaces) <*> pValue

pValue :: CharParser () JValue
pValue = value <* spaces
  where value = JString <$> pString
            <|> JNumber <$> pNumber
            <|> JObject <$> pObject
            <|> JArray  <$> pArray
            <|> JBool   <$> pBool
            <|> JNull   <$ string "null"
            <?> "JSON value"

pBool :: CharParser () Bool
pBool = True <$ string "true"
     <|> False <$ string "false"

pValueChoice = value <* spaces
  where value = choice [ JString <$> pString
                       , JNumber <$> pNumber
                       , JObject <$> pObject
                       , JArray  <$> pArray
                       , JBool   <$> pBool
                       , JNull   <$ string "null"
                       ]
                <?> "JSON value"

-- TODO what is empty
pNumber :: CharParser () Double
pNumber = do 
  num <- many1 (oneOf "0123456789")
  return (read num :: Double)
  -- s <- getInput
  -- case readSigned readFloat s of
  --   [(n, s')] -> n <$ setInput s'
  --   _         -> empty

pString :: CharParser () String
pString = between (char '\"') (char '\"') (many jchar)
    where jchar = char '\\' *> (pEscape <|> pUnicode)
              <|> satisfy (`notElem` "\"\\")

pEscape :: CharParser () Char
pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode :: Char -> Char -> CharParser () Char
          decode c r = r <$ char c

pUnicode :: CharParser () Char
pUnicode = char 'u' *> (decode <$> count 4 hexDigit)
    where decode x = toEnum code
              where ((code,_):_) = readHex x