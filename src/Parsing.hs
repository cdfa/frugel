module Parsing where

import           Node
import           Internal.Meta                  ( defaultMeta )
import           Internal.Program               ( Program(Program) )
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
        , token (preview (_NodeToken % _ExprNode)) Set.empty
          <?> "an expression"
        , Node.identifier <$> Parsing.identifier
        ]

expr :: Parser Expr
expr
    = makeExprParser
        term
        [ [ InfixL $ pure application ]
        , [ InfixL (Sum defaultMeta <$ pToken PlusToken) ]
        ]

decl :: Parser Decl
decl = literalDecl <|> holeDecl
  where
    literalDecl
        = Decl <$> Parsing.identifier <* pToken EqualsToken
        <*> expr
        <*> whereClause
    holeDecl
        = token (preview (_NodeToken % _DeclNode)) Set.empty
        <?> "a declaration"

whereClause :: Parser WhereClause
whereClause
    = WhereClause . concat <$> optional (try (pToken WhereToken *> some decl))

program :: Parser Program
program = Program <$> expr <*> whereClause
