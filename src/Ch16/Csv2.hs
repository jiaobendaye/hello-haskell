module Ch16.Csv2 where

import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line    = sepBy cell (char ',')
cell    = many (noneOf ",\n\r")
-- eol     = char '\n'

-- eol =
--     do char '\n'
--        char '\r' <|> return '\n'

eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    -- <?> "Couldn't find EOL"
    <|> fail "Couldn't find EOL"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)" 
