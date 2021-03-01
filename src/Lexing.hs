{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Lexing where

import           Node
import           ParsingUtils    hiding ( Left, Right )
import qualified ParsingUtils    ( Parenthesis(..) )
import           Text.Megaparsec hiding ( some )
import qualified Data.Set        as Set
import           Data.Char
import           Prettyprinter
import           PrettyPrinting
import           Optics

type Lexer = Parsec Void HoleContents

data LexerToken
    = IdentifierToken Text
    | LambdaToken
    | EqualsToken
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
prettyLexerToken (Parenthesis p) = pretty p
prettyLexerToken (NodeToken node) = annotate OutOfHole $ prettyNode node

char :: Char -> Lexer Char
char c = token (leftToMaybe >=> guarded (== c)) (one . Tokens . one . Left $ c)

alphaNumChar :: Lexer Char
alphaNumChar
    = token (leftToMaybe >=> guarded isAlphaNum) Set.empty
    <?> "an alphanumeric character"

parenthesis :: Lexer Parenthesis
parenthesis
    = (ParsingUtils.Left <$ char '(') <|> (ParsingUtils.Right <$ char ')')

anyNode :: Lexer Node
anyNode = token rightToMaybe Set.empty <?> "a node"

whitespace :: Lexer ()
whitespace
    = void $ takeWhileP (Just "whitespace") (maybe False isSpace . leftToMaybe)

holeContents :: Lexer LexerTokenStream
holeContents
    = fmap fromList . some
    $ (choice
           [ IdentifierToken . toText <$> some alphaNumChar
           , LambdaToken <$ char '\\'
           , EqualsToken <$ char '='
           , Parenthesis <$> parenthesis
           , NodeToken <$> anyNode
           ]
       <* whitespace)
