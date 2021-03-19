{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lexing where

import           Node
import           Parsing.Utils       hiding ( Left, Right )
import qualified Parsing.Utils       ( Parenthesis(..) )
import           Text.Megaparsec
import           Data.Char
import           Prettyprinter
import           PrettyPrinting.Text
import           Optics

type Lexer = Parsec Void HoleContents

data LexerToken
    = IdentifierToken Text
    | LambdaToken
    | EqualsToken
    | WhereToken
    | PlusToken
    | EmptyHoleToken
    | Parenthesis Parenthesis
    | WhitespaceToken Text
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

prettyLexerToken :: LexerToken -> Doc Annotation
prettyLexerToken (IdentifierToken name) = pretty name
prettyLexerToken LambdaToken = "\\"
prettyLexerToken EqualsToken = "="
prettyLexerToken WhereToken = "where"
prettyLexerToken PlusToken = "+"
prettyLexerToken EmptyHoleToken = "???"
prettyLexerToken (Parenthesis p) = pretty p
prettyLexerToken (WhitespaceToken _) = "<whitespace>"
prettyLexerToken (NodeToken node) = outOfHole $ prettyNode node

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
    = (Parsing.Utils.Left <$ char '(') <|> (Parsing.Utils.Right <$ char ')')

anyNode :: Lexer Node
anyNode = namedToken "a node" rightToMaybe

whitespace :: Lexer Text
whitespace
    = fmap toText . some
    $ namedToken "<whitespace>" (leftToMaybe >=> guarded isSpace)

holeContents :: Lexer LexerTokenStream
holeContents
    = fmap fromList . some
    $ choice
        [ LambdaToken <$ char '\\'
        , EqualsToken <$ char '='
        , WhereToken <$ string "where"
        , PlusToken <$ char '+'
        , EmptyHoleToken <$ string "???"
        , Parenthesis <$> parenthesis
        , NodeToken <$> anyNode
        , IdentifierToken . toText <$> some alphaNumChar
        , WhitespaceToken <$> whitespace
        ]
