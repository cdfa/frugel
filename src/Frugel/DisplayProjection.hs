{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Frugel.DisplayProjection
    ( module Frugel.DisplayProjection
    , module Prettyprinter
    ) where

import Data.Data

import Frugel.Decomposition
import Frugel.Node
import Frugel.Program

import Optics

import Prettyprinter

data CompletionStatus = InConstruction | Complete
    deriving ( Show, Eq, Data )

data Annotation
    = CompletionAnnotation CompletionStatus
    | Cursor -- Cursor is only supposed to be inserted into SimpleDocStream after layout. Any contents will be discarded.
    deriving ( Show, Eq, Data )

annotateInConstruction, annotateComplete :: Doc Annotation -> Doc Annotation
annotateInConstruction = annotate $ CompletionAnnotation InConstruction

annotateComplete = annotate $ CompletionAnnotation Complete

class DisplayProjection a where
    renderDoc :: a -> Doc Annotation
    default renderDoc
        :: (Decomposable a, CstrSiteNode a, DisplayProjection (NodeOf a))
        => a
        -> Doc Annotation
    renderDoc = defaultRenderDoc

instance DisplayProjection Program

instance DisplayProjection n => DisplayProjection (ACstrSite n) where
    renderDoc = prettyCstrSite renderDoc

instance DisplayProjection Node where
    -- _NodeCstrSite of Node finds construction sites from the nodes and would skip any overridden renderDoc definitions
    renderDoc (ExprNode expr) = renderDoc expr
    renderDoc (DeclNode decl) = renderDoc decl
    renderDoc (WhereNode w) = renderDoc w

instance DisplayProjection Expr where
    -- Parentheses are handled here (instead of relying on decompose), because the information would be removed when the expression is a construction site
    renderDoc = either defaultRenderDoc parenthesize . unwrapParentheses
      where
        parenthesize (leadingFragment, e, trailingFragment)
            = parens (pretty leadingFragment
                      <> renderDoc e
                      <> pretty trailingFragment)

instance DisplayProjection Decl

instance DisplayProjection WhereClause

defaultRenderDoc
    :: (Decomposable s, DisplayProjection (NodeOf s), CstrSiteNode s)
    => s
    -> Doc Annotation
defaultRenderDoc x
    = maybe (renderDecomposable x) (prettyCstrSite renderDoc)
    $ preview _NodeCstrSite x

renderDecomposable
    :: (Decomposable a, DisplayProjection (NodeOf a)) => a -> Doc Annotation
renderDecomposable
    = foldMap (either pretty renderDoc) . view _CstrSite . decompose

-- Invariant: prettyCstrSite of a non-empty Seq results in a non-empty Doc
prettyCstrSite :: (n -> Doc Annotation) -> ACstrSite n -> Doc Annotation
prettyCstrSite prettyNode (CstrSite contents)
    = annotateInConstruction
    . foldMap (either pretty (foldMap (annotateComplete . prettyNode)))
    . groupByEither
    $ toList contents

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = x <> nest 4 (softline <> y)