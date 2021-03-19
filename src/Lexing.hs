module Lexing where

import           Node
import           Parsing.Utils   hiding ( Left, Right )
import           Text.Megaparsec
import           Data.Char

char :: Char -> Parser Char
char c = token (leftToMaybe >=> guarded (== c)) (one . Tokens . one . Left $ c)

string :: String -> Parser String
string s = s <$ chunk (fromList $ map Left s)

alphaNumChar :: Parser Char
alphaNumChar
    = namedToken
        "an alphanumeric character"
        (leftToMaybe >=> guarded isAlphaNum)

anyNode :: Parser Node
anyNode = namedToken "a node" rightToMaybe
