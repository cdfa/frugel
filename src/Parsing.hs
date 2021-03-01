module Parsing where

import           Node
import           Lexing
import           ParsingUtils                   hiding ( Left, Right )
import qualified ParsingUtils                   ( Parenthesis(..) )
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import qualified Data.Set                       as Set
import           Optics

type Parser = Parsec Void LexerTokenStream

identifier :: Parser Text
identifier = token (preview _IdentifierToken) Set.empty <?> "an identifier"

term :: Parser Node
term
    = choice
        [ abstraction <$ pToken LambdaToken <*> Parsing.identifier
          <* pToken EqualsToken
          <*> expr
        , set (nodeMeta % #parenthesized) False
          <$ pToken (Parenthesis ParsingUtils.Left)
          <*> expr
          <* pToken (Parenthesis ParsingUtils.Right)
          -- Non recursive production rules at the bottom
        , token (preview _NodeToken) Set.empty <?> "a node"
        , Node.identifier <$> Parsing.identifier
        ]

expr :: Parser Node
expr = makeExprParser term [ [ InfixL $ pure application ] ]
