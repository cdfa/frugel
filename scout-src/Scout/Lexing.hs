{-# LANGUAGE TypeFamilies #-}

module Scout.Lexing where

import Data.Alphanumeric
import Data.Char
import qualified Data.Set as Set

import Prelude       hiding ( some )

import Scout.Node
import Scout.Parsing.Error

import Text.Megaparsec

type Parser = Parsec ScoutParseError CstrSite

namedToken :: MonadParsec e s m => String -> (Token s -> Maybe a) -> m a
namedToken name test = token test Set.empty <?> name

char :: Char -> Parser Char
char c = c <$ single (Left c)

string :: String -> Parser String
string s = s <$ chunk (fromList $ map Left s)

alphaNumChar :: Parser Alphanumeric
alphaNumChar
    = namedToken "an alphanumeric character" (leftToMaybe >=> fromChar)

letter :: Parser Alphanumeric
letter
    = namedToken "a letter in the alphabet"
                 (leftToMaybe >=> guarded isLetter >=> fromChar)

decimal :: (MonadParsec e s m, Token s ~ Either Char Node, Num a) => m a
decimal = label "integer" $ foldl' step 0 <$> some digit
  where
    step a c = a * 10 + fromIntegral (digitToInt c)

digit :: (MonadParsec e s m, Token s ~ Either Char Node) => m Char
digit = namedToken "digit" (leftToMaybe >=> guarded isDigit)
