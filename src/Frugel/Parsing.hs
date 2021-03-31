{-# LANGUAGE FlexibleContexts #-}

module Frugel.Parsing where

import           Control.Monad.Combinators.Expr

import           Frugel.Lexing
import           Frugel.Meta
import           Frugel.Node                    as Node
import           Frugel.Parsing.Utils           hiding ( Left, Right )
import qualified Frugel.Parsing.Utils
import           Frugel.Parsing.Whitespace
import           Frugel.Program                 as Program

import           Optics

import           Text.Megaparsec

identifier :: Parser Text
identifier = toText <$> some alphaNumChar <?> "an identifier"

node :: String -> Prism' Node w -> Parser w
node name nodePrism = namedToken name $ preview (_Right % nodePrism)

term :: Parser Expr
term
    = setWhitespace
    <$> choice
        [ abstraction <$% char '\\' <*%> Frugel.Parsing.identifier <*% char '='
          <*%> expr
        , ((exprMeta % #parenthesisLevels) +~ 1)
          <$% (Frugel.Parsing.Utils.Left <$ char '(')
          <*%> expr
          <*% (Frugel.Parsing.Utils.Right <$ char ')')
          -- Non recursive production rules at the bottom
        , exprCstrSite (CstrMaterials empty) <$% string "..."
        , noWhitespace <$> node "an expression node" _ExprNode
        , Node.identifier <$%> Frugel.Parsing.identifier
        ]

expr :: Parser Expr
expr
    = makeExprParser
        term
        [ [ InfixL
            $ try
                (Application . setWhitespace
                 <$> (defaultExprMeta <<$> Frugel.Parsing.Whitespace.whitespace
                      <* notFollowedBy -- Ugly lookahead to fix problem of succeeding on whitespace between expression and +. Fixable by indentation sensitive parsing, but that requires a TraversableStream instance (or rebuilding the combinators)
                          ((() <$ char '+')
                           <|> (() <$ string "where")
                           <|> snd
                           <$> (() <$% Frugel.Parsing.identifier <*% char '='))))
          ]
        , [ InfixL
            $ try
                (Sum . setWhitespace
                 <$> (defaultExprMeta <$% pure () <*% char '+' <*% pure ()))
          ]
        ]

decl :: Parser Decl
decl = setWhitespace <$> (literalDecl <|> cstrSiteDecl)
  where
    literalDecl
        = Node.decl <$%> Frugel.Parsing.identifier <*% char '=' <*%> expr -- <*%> whereClause
    cstrSiteDecl = noWhitespace <$> node "a declaration node" _DeclNode

whereClause :: Parser WhereClause
whereClause = setWhitespace <$> (cstrSiteWhere <|> literalWhere) -- it's important that cstrSiteWhere is tried first, because literalWhere succeeds on empty input
  where
    literalWhere
        = (Node.whereClause . concat)
        <<$>> wOptional (string "where" *%> wSome Frugel.Parsing.decl)
    cstrSiteWhere = noWhitespace <$> node "a declaration node" _WhereNode

program :: Parser Program
program
    = setProgramWhitespace
    <$> (Program.program <$%> expr <*%> Frugel.Parsing.whereClause <*% pure ())
  where
    setProgramWhitespace :: WithWhitespace Program -> Program
    setProgramWhitespace (whitespaceFragments :> trailingWhitespace, p)
        = set (#meta % #trailingWhitespace) trailingWhitespace
        $ setWhitespace (whitespaceFragments, p)
    setProgramWhitespace _ = error "not enough whitespace fragments"

parseCstrSite :: FilePath
    -> CstrMaterials
    -> Either (NonEmpty (ParseError CstrMaterials Void)) Program
parseCstrSite filePath cstrMaterials
    = first bundleErrors
    $ runParser (Frugel.Parsing.program <* eof) filePath cstrMaterials
