module Parsing where

import           Node
import           Program
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

term :: Parser Expr
term
    = choice
        [ abstraction <$ pToken LambdaToken <*> Parsing.identifier
          <* pToken EqualsToken
          <*> expr
        , set (exprMeta % #parenthesized) False
          <$ pToken (Parenthesis ParsingUtils.Left)
          <*> expr
          <* pToken (Parenthesis ParsingUtils.Right)
          -- Non recursive production rules at the bottom
        , token (preview _NodeToken) Set.empty <?> "an expression"
        , Node.identifier <$> Parsing.identifier
        ]

expr :: Parser Expr
expr = makeExprParser term [ [ InfixL $ pure application ] ]

program :: Parser Program
program = expr
