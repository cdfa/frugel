module Frugel.Lexing where

import           Data.Char

import           Frugel.Node
import           Frugel.Parsing.Utils hiding ( Left, Right )

import           Text.Megaparsec

char :: Char -> Parser Char
char c = token (leftToMaybe >=> guarded (== c)) (one . Tokens . one . Left $ c)

string :: String -> Parser String
string s = s <$ chunk (fromList $ map Left s)

alphaNumChar :: Parser Char
alphaNumChar
    = namedToken
        "an alphanumeric character"
        (leftToMaybe >=> guarded isAlphaNum)

lowerChar :: Parser Char
lowerChar
    = namedToken "a lower-case character" (leftToMaybe >=> guarded isLower)

anyNode :: Parser Node
anyNode = namedToken "a node" rightToMaybe
