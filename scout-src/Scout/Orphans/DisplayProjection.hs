{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Scout.Orphans.DisplayProjection where

import Frugel

import Scout.Node
import Scout.PrettyPrinting
import Scout.Truncatable

-- Orphaned because of dependency on pretty printing
instance DisplayProjection EvaluationError where
    renderDoc = \case
        TypeError e -> "Type error:" <+> renderDoc e
        FreeVariableError name -> dquotes (pretty name) <+> "was not defined"
        ConflictingDefinitionsError name -> dquotes (pretty name)
            <+> "was defined multiple times in a the same scope"
        OutOfFuelError expr -> "Ran out of fuel when evaluating:"
            `nestingLine` annotateComplete
                (reAnnotate toStandardAnnotation . annPretty $ truncate 5 expr)
        DivideByZeroError -> "Divide-by-zero error"

instance DisplayProjection TypeError where
    renderDoc = \case
        TypeValueMismatch expected expr -> "Expected type"
            <+> pretty expected <> line <> "does not match"
            <+> annotateComplete
                (reAnnotate toStandardAnnotation . annPretty $ truncate 5 expr)
        LiteralTypesMismatch e1 e2 -> "Could not match types of"
            <+> renderDoc e1
            <+> "and"
            <+> renderDoc e2

instance Pretty ExpectedType where
    pretty = \case
        FunctionType -> "Function"
        IntegerType -> "Integer"
        BoolType -> "Boolean"
        AnyType -> "Any"
