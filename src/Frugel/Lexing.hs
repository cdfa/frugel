module Frugel.Lexing where

import           Data.Char
import qualified Data.Set        as Set

import           Frugel.Node

import           Text.Megaparsec

type Parser = Parsec Void CstrSite

namedToken :: MonadParsec e s m => String -> (Token s -> Maybe a) -> m a
namedToken name test = token test Set.empty <?> name

char :: Char -> Parser Char
char c = c <$ single (Left c)

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
