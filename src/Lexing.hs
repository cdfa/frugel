{-# LANGUAGE TypeFamilies #-}

module Lexing where

import           Node
import           ParsingUtils    hiding ( Left, Right )
import qualified ParsingUtils    ( Parenthesis(..) )
import           Text.Megaparsec hiding ( some )
import qualified Data.Set        as Set
import           Data.Char

type Lexer = Parsec Void HoleContents

data LexerToken
    = IdentifierToken Text
    | LambdaToken
    | EqualsToken
    | Parenthesis Parenthesis
    | NodeToken Node
    deriving ( Eq, Ord, Show )

nodeTokenToNode :: LexerToken -> Maybe Node
nodeTokenToNode (NodeToken node) = Just node
nodeTokenToNode _ = Nothing

identifierTokenToText :: LexerToken -> Maybe Text
identifierTokenToText (IdentifierToken text) = Just text
identifierTokenToText _ = Nothing

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

holeContents :: Lexer (Seq LexerToken)
holeContents
    = fmap fromList . some
    $ choice
        [ IdentifierToken . toText <$> some alphaNumChar
        , LambdaToken <$ char '\\'
        , EqualsToken <$ char '='
        , Parenthesis <$> parenthesis
        , NodeToken <$> anyNode
        ]
-- instance Stream HoleContents where
--     type Token HoleContents = WithPos LexerToken
--     type Tokens HoleContents = [WithPos LexerToken]
--     tokensToChunk Proxy = id
--     chunkToTokens Proxy = id
--     chunkLength Proxy = length
--     chunkEmpty Proxy = null
