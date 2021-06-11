{-# LANGUAGE LambdaCase #-}

module Frugel.DisplayProjection where

import Frugel.Decomposition
import Frugel.Error
import Frugel.Node
import Frugel.Parsing.Error
import Frugel.PrettyPrinting
import Frugel.Program

import Optics

class DisplayProjection a where
    displayDoc :: a -> Doc Annotation

instance DisplayProjection CstrSite where
    displayDoc = prettyCstrSite displayDoc

instance DisplayProjection Node where
    displayDoc (ExprNode expr) = displayDoc expr
    displayDoc (DeclNode decl) = displayDoc decl
    displayDoc (WhereNode w) = displayDoc w

instance DisplayProjection Identifier where
    displayDoc identifier = decomposableDoc identifier

instance DisplayProjection Expr where
    -- Parentheses are handled here (instead of relying on decompose), because the information would be removed when the expression is a construction site
    displayDoc = either displayDoc' parenthesize . unwrapParentheses
      where
        parenthesize (leadingFragment, e, trailingFragment)
            = parens (pretty leadingFragment
                      <> displayDoc e
                      <> pretty trailingFragment)
        displayDoc' (ExprCstrSite _ contents) = displayDoc contents
        displayDoc' expr = decomposableDoc expr

instance DisplayProjection Decl where
    displayDoc (DeclCstrSite _ materials) = displayDoc materials
    displayDoc decl = decomposableDoc decl

instance DisplayProjection WhereClause where
    displayDoc (WhereCstrSite _ materials) = displayDoc materials
    displayDoc whereClause = decomposableDoc whereClause

instance DisplayProjection Program where
    displayDoc (ProgramCstrSite _ materials) = displayDoc materials
    displayDoc program = decomposableDoc program

instance DisplayProjection Error where
    displayDoc = \case
        ParseError e -> parseErrorPretty e
        InternalError e -> "Internal error:" <+> displayDoc e

instance DisplayProjection InternalError where
    displayDoc = \case
        ParseFailedAfterPrettyPrint
         -> "failed to reparse a pretty-printed program"
        ASTModificationNotPerformed cursorOffset ->
            "AST was not modified. Cursor offset:" <+> show cursorOffset
        DecompositionFailed cursorOffset ->
            "failed to decompose AST for cursor offset" <+> show cursorOffset
        CstrSiteActionFailed cstrSiteOffset cstrSite ->
            "failed to modify the construction site"
            `nestingLine` displayDoc cstrSite
            <> line
            <> "at index"
            <+> show cstrSiteOffset

decomposableDoc :: Decomposable a => a -> Doc Annotation
decomposableDoc
    = foldMap (either pretty displayDoc) . view _CstrSite . decompose