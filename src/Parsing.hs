module Parsing where

import           Node
import           Internal.Meta                  ( defaultMeta )
import           Internal.Program               ( Program(Program) )
import           Lexing
import           ParsingUtils                   hiding ( Left, Right )
import qualified ParsingUtils                   ( Parenthesis(..) )
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import           Optics

type Parser = Parsec Void LexerTokenStream

identifier :: Parser Text
identifier = namedToken "an identifier" $ preview _IdentifierToken

node :: String -> Prism' Node w -> Parser w
node name nodePrism = namedToken name $ preview (_NodeToken % nodePrism)

term :: Parser Expr
term
    = choice
        [ abstraction <$ literalToken LambdaToken <*> Parsing.identifier
          <* literalToken EqualsToken
          <*> expr
        , set (exprMeta % #parenthesized) False
          <$ literalToken (Parenthesis ParsingUtils.Left)
          <*> expr
          <* literalToken (Parenthesis ParsingUtils.Right)
          -- Non recursive production rules at the bottom
        , hole (HoleContents empty) <$ literalToken EmptyHoleToken
        , node "an expression" _ExprNode
        , Node.identifier <$> Parsing.identifier
        ]

expr :: Parser Expr
expr
    = makeExprParser
        term
        [ [ InfixL $ pure application ]
        , [ InfixL (Sum defaultMeta <$ literalToken PlusToken) ]
        ]

decl :: Parser Decl
decl = literalDecl <|> holeDecl
  where
    literalDecl
        = Decl <$> Parsing.identifier <* literalToken EqualsToken <*> expr
        -- <*> whereClause
    holeDecl = node "a declaration" _DeclNode

whereClause :: Parser WhereClause
whereClause = literalDecl <|> holeDecl
  where
    literalDecl
        = WhereClause . concat
        <$> optional (try (literalToken WhereToken *> some decl))
    holeDecl = node "a declaration" _WhereNode

program :: Parser Program
program = Program <$> expr <*> whereClause
