{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Scout.PrettyPrinting where

import Data.Has

import Frugel.DisplayProjection

import Optics.Extra.Scout

import PrettyPrinting.Expr

import qualified Scout.Internal.Node
import Scout.Node         hiding ( Elided )

data PrettyAnnotation = CompletionAnnotation' Node CompletionStatus | Elided'

toStandardAnnotation :: PrettyAnnotation -> Annotation
toStandardAnnotation (CompletionAnnotation' _ completionStatus)
    = CompletionAnnotation completionStatus
toStandardAnnotation Elided' = Elided

prettyCstrSite :: Node
    -> (Node -> Doc PrettyAnnotation)
    -> CstrSite
    -> Doc PrettyAnnotation
prettyCstrSite n
    = renderCstrSite' (annotateInConstruction' n) annotateComplete'

annotateInConstruction' :: Node -> Doc PrettyAnnotation -> Doc PrettyAnnotation
annotateInConstruction' n = annotate $ CompletionAnnotation' n InConstruction

annotateComplete' :: Node -> Doc PrettyAnnotation -> Doc PrettyAnnotation
annotateComplete' n = annotate $ CompletionAnnotation' n Complete

class AnnotatedPretty a where
    annPretty :: a -> Doc PrettyAnnotation

instance AnnotatedPretty Node where
    annPretty (ExprNode expr) = annPretty expr
    annPretty (DeclNode decl) = annPretty decl
    annPretty (WhereNode w) = annPretty w

instance AnnotatedPretty Expr where
    annPretty
        = stubIfNotEvaluated
        . prettyNodeWithMeta
        . parenthesizeExprFromMeta parens
        $ \expr -> case expr of
            Variable _ n -> pretty n
            Abstraction _ arg subExp -> backslash
                <> pretty arg
                `nestingLine` (equals <+> annPretty (prettyUnary expr subExp))
            Application _ function arg -> uncurry nestingLine
                $ both %~ annPretty
                $ prettyBinary expr function arg
            IfExpression _ conditional trueExpr falseExpr -> align
                $ vsep [ "if" <+> annPretty conditional
                       , "then" <+> annPretty trueExpr
                       , "else" <+> annPretty (prettyUnary expr falseExpr)
                       ]
            UnaryOperation _ unOp subExp -> mappend (unaryOperatorSymbol unOp)
                . annPretty
                $ prettyUnary expr subExp
            BinaryOperation _ left binOp right ->
                (\(left', right') -> annPretty left'
                 `nestingLine` (binaryOperatorSymbol binOp
                                <+> annPretty right'))
                $ prettyBinary expr left right
            Literal _ l -> pretty l
            ExprCstrSite _ contents ->
                prettyCstrSite (ExprNode expr) annPretty contents
      where
        (prettyUnary, prettyBinary)
            = makeExprPrettyPrinter (exprMeta % #parenthesisLevels %~ max 1)
        stubIfNotEvaluated prettyNode n
            = case n ^. hasLens @ExprMeta % #evaluationStatus of
                Evaluated -> prettyNode n
                evaluationStatus -> annotate Elided' $ pretty evaluationStatus

instance AnnotatedPretty Decl where
    annPretty = prettyNodeWithMeta $ \case
        Decl{..} -> pretty name `nestingLine` equals <+> annPretty value
        d@(DeclCstrSite _ contents) ->
            prettyCstrSite (DeclNode d) annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty = prettyNodeWithMeta $ \case
        (WhereClause _ decls) -> "where"
            <> nest 2 (line <> vsep (map annPretty $ toList decls))
        whereClause@(WhereCstrSite _ contents) ->
            prettyCstrSite (WhereNode whereClause) annPretty contents

prettyNodeWithMeta
    :: Has Meta n => (n -> Doc PrettyAnnotation) -> n -> Doc PrettyAnnotation
prettyNodeWithMeta prettyNode n
    = if getter @Meta n ^. #elided
      then annotate Elided' "..."
      else prettyNode n
