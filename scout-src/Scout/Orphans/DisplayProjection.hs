{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Scout.Orphans.DisplayProjection where

import Frugel

import Scout.Node
import Scout.PrettyPrinting

-- Orphaned because of dependency on pretty printing
instance DisplayProjection EvaluationError where
    renderDoc = \case
        TypeError e -> "Type error:" <+> renderDoc e
        UnboundVariableError name -> dquotes (pretty name)
            <+> "was not defined"
        ConflictingDefinitionsError name -> dquotes (pretty name)
            <+> "was defined multiple times in a the same scope"
        OutOfFuelError expr -> "Ran out of fuel when evaluating:"
            `nestingLine` annotateComplete
                (reAnnotate toStandardAnnotation $ annPretty expr)

instance DisplayProjection TypeError where
    renderDoc (TypeMismatchError expected expr)
        = "Expected type"
        <+> pretty expected <> line <> "does not match"
        <+> annotateComplete (reAnnotate toStandardAnnotation $ annPretty expr)

instance Pretty ExpectedType where
    pretty = viaShow
