{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Error
    ( module Frugel.Error
    , module Frugel.Error.InternalError
    ) where

import Frugel.CstrSite
import Frugel.DisplayProjection
import Frugel.Error.InternalError
import Frugel.ParserOf
import Frugel.Parsing.Error
import Frugel.Program

data Error p = ParseError (ParseErrorOf p) | InternalError (InternalError p)

instance DisplayProjection (Error Program) where
    renderDoc = \case
        ParseError e -> parseErrorPretty e
        InternalError e -> "Internal error:" <+> renderDoc e

instance DisplayProjection (NodeOf p)
    => DisplayProjection (InternalError p) where
    renderDoc = \case
        ParseFailedAfterPrettyPrint
         -> "failed to reparse a pretty-printed program"
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
