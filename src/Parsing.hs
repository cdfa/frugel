module Parsing where

import           Node
import           Lexing
import           ParsingUtils                   hiding ( Left, Right )
import qualified ParsingUtils                   ( Parenthesis(..) )
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import qualified Data.Set                       as Set

type Parser = Parsec Void (Seq LexerToken)

identifier :: Parser Text
identifier = token identifierTokenToText Set.empty <?> "an identifier"

term :: Parser Node
term
    = choice
        [ Abstraction <$ pToken LambdaToken <*> identifier
          <* pToken EqualsToken
          <*> expr
        , Parenthesized <$ pToken (Parenthesis ParsingUtils.Left) <*> expr
          <* pToken (Parenthesis ParsingUtils.Right)
          -- Non recursive production rules at the bottom
        , token nodeTokenToNode Set.empty <?> "a node"
        , Identifier <$> identifier
        ]

expr :: Parser Node
expr = makeExprParser term [ [ InfixL $ pure Application ] ]
