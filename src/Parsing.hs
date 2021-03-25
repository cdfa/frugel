{-# LANGUAGE FlexibleContexts #-}

module Parsing where

import           Node
import           Internal.Program               as Program ( Program, program )
import           Lexing
import           Parsing.Utils                  hiding ( Left, Right )
import qualified Parsing.Utils
import           Parsing.Whitespace
import           Text.Megaparsec
import           Control.Monad.Combinators.Expr
import           Optics
import           Internal.Meta                  ( defaultExprMeta )

identifier :: Parser Text
identifier = toText <$> some alphaNumChar <?> "an identifier"

node :: String -> Prism' Node w -> Parser w
node name nodePrism = namedToken name $ preview (_Right % nodePrism)

term :: Parser Expr
term
    = setWhitespace
    <$> choice
        [ abstraction <$% char '\\' <*%> Parsing.identifier <*% char '='
          <*%> expr
        , ((exprMeta % #parenthesisLevels) +~ 1)
          <$% (Parsing.Utils.Left <$ char '(')
          <*%> expr
          <*% (Parsing.Utils.Right <$ char ')')
          -- Non recursive production rules at the bottom
        , exprCstrSite (CstrMaterials empty) <$% string "???"
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
                          ((() <$ char '+')
                           <|> (() <$ string "where")
                           <|> snd <$> (() <$% Parsing.identifier <*% char '='))))
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
    literalDecl = Node.decl <$%> Parsing.identifier <*% char '=' <*%> expr -- <*%> whereClause
    cstrSiteDecl = noWhitespace <$> node "a declaration node" _DeclNode

whereClause :: Parser WhereClause
whereClause = setWhitespace <$> (literalDecl <|> cstrSiteDecl)
  where
    literalDecl
        = (Node.whereClause . concat)
        <<$>> wOptional (string "where" *%> wSome Parsing.decl)
    cstrSiteDecl = noWhitespace <$> node "a declaration node" _WhereNode

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

parseCstrSite :: FilePath
    -> CstrMaterials
    -> Either (NonEmpty (ParseError CstrMaterials Void)) Program
parseCstrSite filePath cstrMaterials
    = first bundleErrors
    $ runParser (Parsing.program <* eof) filePath cstrMaterials
