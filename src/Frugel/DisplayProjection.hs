{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.DisplayProjection
    ( module Frugel.DisplayProjection
    , module Prettyprinter
    ) where

import Data.Data

import Frugel.CstrSite
import Frugel.Decomposition
import Frugel.Error.InternalError

import Optics.Extra

import Prettyprinter
import Prettyprinter.Internal.Type ( Doc(Char) )

data CompletionStatus = InConstruction | Complete
    deriving ( Show, Eq, Data )

data Annotation' s
    = CompletionAnnotation s
    | Cursor -- Cursor is only supposed to be inserted into SimpleDocStream after layout. Any contents will be discarded.
    deriving ( Show, Eq, Data )

type Annotation = Annotation' CompletionStatus

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

instance DisplayProjection (NodeOf p)
    => DisplayProjection (InternalError p) where
    renderDoc = \case
        ASTModificationNotPerformed cursorOffset ->
            "AST was not modified. Cursor offset:" <+> show cursorOffset
        DecompositionFailed cursorOffset ->
            "failed to decompose AST for cursor offset" <+> show cursorOffset
        CstrSiteActionFailed cstrSiteOffset cstrSite ->
            "failed to modify the construction site"
            `nestingLine` renderDoc cstrSite
            <> line
            <> "at index"
            <+> show cstrSiteOffset

instance DisplayProjection n => DisplayProjection (ACstrSite n) where
    renderDoc = renderCstrSite renderDoc

defaultRenderDoc
    :: (Decomposable s, DisplayProjection (NodeOf s), CstrSiteNode s)
    => s
    -> Doc Annotation
defaultRenderDoc x
    = maybe (renderDecomposable x) (renderCstrSite renderDoc)
    $ preview _NodeCstrSite x

renderDecomposable
    :: (Decomposable a, DisplayProjection (NodeOf a)) => a -> Doc Annotation
renderDecomposable
    = foldMap (either pretty renderDoc) . view _CstrSite . decompose

renderCstrSite :: (n -> Doc Annotation) -> ACstrSite n -> Doc Annotation
renderCstrSite = renderCstrSite' annotateInConstruction annotateComplete

-- Invariant: renderCstrSite of a non-empty Seq results in a non-empty Doc
renderCstrSite' :: (Doc a -> Doc a)
    -> (Doc a -> Doc a)
    -> (n -> Doc a)
    -> ACstrSite n
    -> Doc a
renderCstrSite' inConstruction complete prettyNode (CstrSite contents)
    = inConstruction . foldMap (either prettyChar (complete . prettyNode))
    $ toList contents
  where
    prettyChar '\n' = Char '\n' -- explicitly insert newlines as Char to to prevent insertion of extra whitespace when pretty printing construction sites which are `nest`ed
    prettyChar c = pretty c

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = x <> nest 4 (softline <> y)