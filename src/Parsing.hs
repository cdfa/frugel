{-# LANGUAGE FlexibleContexts #-}

module Parsing where

import           Node
import           Internal.Program               as Program ( Program, program )
import           Lexing
import           Parsing.Utils                  hiding ( Left, Right )
import qualified Parsing.Utils                  ( Parenthesis(..) )
import           Parsing.Whitespace
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import           Optics
import           Internal.Meta                  ( defaultExprMeta )

type Parser = Parsec Void LexerTokenStream

identifier :: Parser Text
identifier = namedToken "an identifier" $ preview _IdentifierToken

node :: String -> Prism' Node w -> Parser w
node name nodePrism = namedToken name $ preview (_NodeToken % nodePrism)

term :: Parser Expr
term
    = setWhitespace
    <$> choice
        [ abstraction <$% literalToken LambdaToken <*%> Parsing.identifier
          <*% literalToken EqualsToken
          <*%> expr
        , ((exprMeta % #parenthesisLevels) +~ 1)
          <$% literalToken (Parenthesis Parsing.Utils.Left)
          <*%> expr
          <*% literalToken (Parenthesis Parsing.Utils.Right)
          -- Non recursive production rules at the bottom
        , hole (HoleContents empty) <$% literalToken EmptyHoleToken
        , noWhitespace <$> node "an expression node" _ExprNode
        , Node.identifier <$%> Parsing.identifier
        ]

expr :: Parser Expr
expr
    = makeExprParser
        term
        [ [ InfixL
            $ try
                (Application . setWhitespace
                 <$> (defaultExprMeta <<$> Parsing.Whitespace.whitespace
                      <* notFollowedBy -- Ugly lookahead to fix problem of succeeding on whitespace between expression and +. Fixable by indentation sensitive parsing, but that requires a TraversableStream instance (or rebuilding the combinators)
                          (literalToken PlusToken
                           <|> literalToken WhereToken
                           <|> snd
                           <$> (IdentifierToken <$%> Parsing.identifier
                                <*% literalToken EqualsToken))))
          ]
        , [ InfixL
            $ try
                (Sum . setWhitespace
                 <$> (defaultExprMeta <$% pure ()
                      <*% literalToken PlusToken
                      <*% pure ()))
          ]
        ]

decl :: Parser Decl
decl = setWhitespace <$> (literalDecl <|> holeDecl)
  where
    literalDecl
        = Node.decl <$%> Parsing.identifier <*% literalToken EqualsToken
        <*%> expr -- <*%> whereClause
    holeDecl = noWhitespace <$> node "a declaration node" _DeclNode

whereClause :: Parser WhereClause
whereClause = setWhitespace <$> (literalDecl <|> holeDecl)
  where
    literalDecl
        = (Node.whereClause . concat)
        <<$>> wOptional (literalToken WhereToken *%> wSome Parsing.decl)
    holeDecl = noWhitespace <$> node "a declaration node" _WhereNode

program :: Parser Program
program
    = setProgramWhitespace
    <$> (Program.program <$%> expr <*%> Parsing.whereClause <*% pure ())
  where
    setProgramWhitespace :: WithWhitespace Program -> Program
    setProgramWhitespace (whitespaceFragments :> trailingWhitespace, p)
        = set (#meta % #trailingWhitespace) trailingWhitespace
        $ setWhitespace (whitespaceFragments, p)
    setProgramWhitespace _ = error "not enough whitespace fragments"
