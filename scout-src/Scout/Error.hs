{-# LANGUAGE LambdaCase #-}

module Scout.Error where

import qualified Frugel
import Frugel.DisplayProjection

import Optics

import Scout.Node
import Scout.Parsing.Error
import Scout.Program

data Error
    = EvaluationError Int EvaluationError
    | ParseError ParseError
    | InternalError (Frugel.InternalError Program)
    deriving ( Show, Eq )

instance DisplayProjection Error where
    renderDoc = \case
        EvaluationError count e -> "Evaluation error"
            <> plural mempty (" (occurring" <+> pretty count <+> "times") count
            <> ":"
            <+> renderDoc e
        ParseError e -> parseErrorPretty e
        InternalError e -> "Internal error:" <+> renderDoc e

fromFrugelError :: Frugel.Error Program -> Error
fromFrugelError (Frugel.ParseError e) = ParseError e
fromFrugelError (Frugel.InternalError e) = InternalError e

matchFrugelError :: Error -> Either (Frugel.Error Program) Error
matchFrugelError (ParseError e) = Left $ Frugel.ParseError e
matchFrugelError (InternalError e) = Left $ Frugel.InternalError e
matchFrugelError e@EvaluationError{} = Right e

_FrugelError :: Prism' Error (Frugel.Error Program)
_FrugelError = prism' fromFrugelError (leftToMaybe . matchFrugelError)
