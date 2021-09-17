module Scout.Lexing where

import Data.Alphanumeric
import qualified Data.Set as Set

import Scout.Node

import Text.Megaparsec

type Parser = Parsec Void CstrSite

namedToken :: MonadParsec e s m => String -> (Token s -> Maybe a) -> m a
namedToken name test = token test Set.empty <?> name

char :: Char -> Parser Char
char c = c <$ single (Left c)

string :: String -> Parser String
string s = s <$ chunk (fromList $ map Left s)

alphaNumChar :: Parser Alphanumeric
alphaNumChar
    = namedToken "an alphanumeric character" (leftToMaybe >=> fromChar)
-- lowerChar :: Parser Char
-- lowerChar
--     = namedToken "a lower-case character" (leftToMaybe >=> guarded isLower)
