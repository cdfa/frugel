{-# LANGUAGE FlexibleContexts #-}

module Frugel.Parsing
    ( module Frugel.Parsing
    , module Frugel.Parsing.Error
    ) where

import           Control.Monad.Combinators.Expr

import           Data.Composition

import           Frugel.Lexing
import           Frugel.Node
import qualified Frugel.Node                    as Node
import           Frugel.Parsing.Error
import           Frugel.Parsing.Whitespace
import           Frugel.Program                 as Program

import           Optics

import           Text.Megaparsec                hiding ( many )

identifier :: Parser Identifier
identifier = literalIdentifier <|> identifierNode
  where
    literalIdentifier
        = (fromString .: (:)) <$> lowerChar
        <*> many alphaNumChar <?> "an identifier"
    identifierNode = node "a declaration node" _IdentifierNode

node :: String -> Prism' Node w -> Parser w
node name nodePrism = namedToken name $ preview (_Right % nodePrism)

term :: Parser Expr
term
    = choice
        [ surroundOriginalWhitespace
          <$> (char '(' *%> fmap noWhitespace expr <*% char ')')
        , setWhitespace
          <$> choice
              [ Node.abstraction' <$% char '\\' <*%> identifier <*% char '='
                <*%> expr
                -- Non recursive production rules at the bottom
              , exprCstrSite (CstrSite empty) <$% string "..."
              , Node.identifier' <$%> identifier
              ]
        , node "an expression node" _ExprNode
        ]
  where
    surroundOriginalWhitespace
        (whitespaceFragments, e) = case whitespaceFragments of
        [rightFragment, leftFragment] -> e
            & exprMeta % #parenthesisLevels +~ 1
            & exprMeta % #standardMeta % #interstitialWhitespace
            %~ cons leftFragment . flip snoc rightFragment
        _ -> error "Unexpected number of whitespace fragments"

expr :: Parser Expr
expr
    = makeExprParser
        term
        [ [ InfixL
                (Application . setWhitespace
                 <$> try
                     (defaultExprMeta <<$> whitespace
                      <* notFollowedBy -- Ugly lookahead to fix problem of succeeding on whitespace between expression and +. Fixable by indentation sensitive parsing, but that requires a TraversableStream instance (or rebuilding the combinators)
                          (choice
                               [ () <$ char '+'
                               , () <$ string "where"
                               , () <$ node "" _WhereNode
                               , () <$ (() <$% identifier <*% char '=')
                               , () <$ node "" _DeclNode
                               , () <$ char ')'
                               , eof
                               ])))
          ]
        , [ InfixL
                (Sum . setWhitespace
                 <$> try (defaultExprMeta <$% pure () <*% char '+' <*% pure ()))
          ]
        ]

decl :: Parser Decl
decl = setWhitespace <$> literalDecl <|> declNode
  where
    literalDecl = Node.decl' <$%> identifier <*% char '=' <*%> expr -- <*%> whereClause
    declNode = node "a declaration node" _DeclNode

whereClause :: Parser WhereClause
whereClause = whereNode <|> setWhitespace <$> literalWhere -- it's important that whereNode is tried first, because literalWhere succeeds on empty input
  where
    literalWhere = Node.whereClause' <<$>> string "where" *%> wSome decl
    whereNode = node "a where clause node" _WhereNode

program :: Parser Program
program
    = setProgramWhitespace
    <$> (Program.program' <$%> expr <*%> optional whereClause <*% pure ())
  where
    setProgramWhitespace :: WithWhitespace Program -> Program
    setProgramWhitespace
        ( trailingWhitespace : whitespaceFragments -- whitespace fragments are reversed
        , p
        )
        = set (#meta % #trailingWhitespace) trailingWhitespace
        $ setWhitespace (whitespaceFragments, p)
    setProgramWhitespace _ = error "not enough whitespace fragments"

parseCstrSite :: FilePath
    -> CstrSite
    -> Either (NonEmpty (ParseError CstrSite Void)) Program
parseCstrSite filePath cstrSite
    = first bundleErrors $ runParser (program <* eof) filePath cstrSite
