{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lexing where

import           Node
import           ParsingUtils    hiding ( Left, Right )
import qualified ParsingUtils    ( Parenthesis(..) )
import           Text.Megaparsec
import           Data.Char
import           Prettyprinter
import           PrettyPrinting
import           Optics

type Lexer = Parsec Void HoleContents

data LexerToken
    = IdentifierToken Text
    | LambdaToken
    | EqualsToken
    | WhereToken
    | PlusToken
    | Parenthesis Parenthesis
    | NodeToken Node
    deriving ( Eq, Ord, Show )

makePrisms ''LexerToken

newtype LexerTokenStream = LexerTokenStream (Seq LexerToken)
    deriving ( Eq, Ord, Show, Stream, IsList )

instance VisualStream LexerTokenStream where
    showTokens Proxy
        = showTokens (Proxy @String)
        . fromList -- Assumption: prettyHoleContents of a non-empty Seq results in a non-empty render
        . renderSmart
        . foldMap prettyLexerToken

prettyLexerToken :: LexerToken -> Doc HoleAnnotation
prettyLexerToken (IdentifierToken name) = pretty name
prettyLexerToken LambdaToken = "\\"
prettyLexerToken EqualsToken = "="
prettyLexerToken WhereToken = "where"
prettyLexerToken PlusToken = "+"
prettyLexerToken (Parenthesis p) = pretty p
prettyLexerToken (NodeToken node) = annotate OutOfHole $ prettyNode node

char :: Char -> Lexer Char
char c = token (leftToMaybe >=> guarded (== c)) (one . Tokens . one . Left $ c)

string :: String -> Lexer String
string s = s <$ chunk (fromList $ map Left s)

alphaNumChar :: Lexer Char
alphaNumChar
    = namedToken
        "an alphanumeric character"
        (leftToMaybe >=> guarded isAlphaNum)

parenthesis :: Lexer Parenthesis
parenthesis
    = (ParsingUtils.Left <$ char '(') <|> (ParsingUtils.Right <$ char ')')

anyNode :: Lexer Node
anyNode = namedToken "a node" rightToMaybe

whitespace :: Lexer ()
whitespace
    = void $ takeWhileP (Just "whitespace") (maybe False isSpace . leftToMaybe)

holeContents :: Lexer LexerTokenStream
holeContents
    = fmap fromList . some
    $ (choice
           [ LambdaToken <$ char '\\'
           , EqualsToken <$ char '='
           , WhereToken <$ string "where"
           , PlusToken <$ char '+'
           , Parenthesis <$> parenthesis
           , NodeToken <$> anyNode
           , IdentifierToken . toText <$> some alphaNumChar
           ]
       <* whitespace)
