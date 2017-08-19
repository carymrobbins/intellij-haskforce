module Layout00025 where

import           Text.Parsec
import           Text.Parsec.String

testParser :: Parser Char
testParser = do
    c <- char 'c'
    <?> "char c expected"
