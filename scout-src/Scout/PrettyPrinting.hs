{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Scout.PrettyPrinting where

import Data.Has

import Frugel.DisplayProjection

import Optics

import PrettyPrinting.Expr

import qualified Scout.Internal.Node
import Scout.Node

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

instance AnnotatedPretty Identifier where
    annPretty (Identifier name) = pretty name

instance AnnotatedPretty Expr where
    annPretty
        = prettyNodeWithMeta "<ExprNode>" . parenthesizeExprFromMeta parens
        $ \expr -> case expr of
            Variable _ n -> annPretty n
            Abstraction _ arg subExp -> backslash
                <> annPretty arg `nestingLine` equals
                <+> annPretty (prettyUnary expr subExp)
            Application _ function arg -> uncurry nestingLine
                $ both %~ annPretty
                $ prettyBinary expr function arg
            Sum _ left right ->
                (\(left', right') -> annPretty left' `nestingLine` "+"
                 <+> annPretty right') $ prettyBinary expr left right
            ExprCstrSite _ contents ->
                prettyCstrSite (ExprNode expr) annPretty contents
      where
        (prettyUnary, prettyBinary)
            = makeExprPrettyPrinter (exprMeta % #parenthesisLevels %~ max 1)

instance AnnotatedPretty Decl where
    annPretty = prettyNodeWithMeta "<DeclNode>" annPretty'
      where
        annPretty' Decl{..}
            = annPretty name `nestingLine` equals <+> annPretty value
        annPretty' d@(DeclCstrSite _ contents)
            = prettyCstrSite (DeclNode d) annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty = prettyNodeWithMeta "<WhereClauseNode>" annPretty'
      where
        annPretty' (WhereClause _ decls)
            = "where" <> nest 2 (line <> vsep (map annPretty $ toList decls))
        annPretty' whereClause@(WhereCstrSite _ contents)
            = prettyCstrSite (WhereNode whereClause) annPretty contents

prettyNodeWithMeta :: Has Meta n
    => Doc PrettyAnnotation
    -> (n -> Doc PrettyAnnotation)
    -> n
    -> Doc PrettyAnnotation
prettyNodeWithMeta stub prettyNode n
    = if getter @Meta n ^. #elided then annotate Elided' stub else prettyNode n
