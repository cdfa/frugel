{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Scout.PrettyPrinting where

import Frugel.DisplayProjection

import qualified Scout.Internal.Node
import Scout.Node

data PrettyCompletionStatus = InConstruction' Node | Complete'

type PrettyAnnotation = Annotation' PrettyCompletionStatus

prettyCstrSite :: Node
    -> (n -> Doc PrettyAnnotation)
    -> ACstrSite n
    -> Doc PrettyAnnotation
prettyCstrSite n
    = renderCstrSite' (annotateInConstruction' n) annotateComplete'

annotateInConstruction' :: Node -> Doc PrettyAnnotation -> Doc PrettyAnnotation
annotateInConstruction' = annotate . CompletionAnnotation . InConstruction'

annotateComplete' :: Doc PrettyAnnotation -> Doc PrettyAnnotation
annotateComplete' = annotate $ CompletionAnnotation Complete'

class AnnotatedPretty a where
    annPretty :: a -> Doc PrettyAnnotation

instance AnnotatedPretty a => AnnotatedPretty (Maybe a) where
    annPretty = maybe mempty annPretty

instance AnnotatedPretty Node where
    annPretty (ExprNode expr) = annPretty expr
    annPretty (DeclNode decl) = annPretty decl
    annPretty (WhereNode w) = annPretty w

instance AnnotatedPretty Identifier where
    annPretty (Identifier name) = pretty name

instance AnnotatedPretty Expr where
    annPretty = parenthesizeExpr parens annPretty'
      where
        annPretty' (Variable _ n) = annPretty n

        annPretty' (Abstraction _ arg expr)
            = (backslash <> annPretty arg) `nestingLine` equals
            <+> annPretty expr
        annPretty' (Application _ function arg)
            = annPretty function `nestingLine` annPretty arg
        annPretty' (Sum _ left right)
            = annPretty left `nestingLine` "+" <+> annPretty right
        annPretty' e@(ExprCstrSite _ contents)
            = prettyCstrSite (ExprNode e) annPretty contents

instance AnnotatedPretty Decl where
    annPretty Decl{..}
        = annPretty name `nestingLine` equals <+> annPretty value
    annPretty d@(DeclCstrSite _ contents)
        = prettyCstrSite (DeclNode d) annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty (WhereClause _ decls)
        = "where" <> nest 2 (line <> vsep (map annPretty $ toList decls))
    annPretty whereClause@(WhereCstrSite _ contents)
        = prettyCstrSite (WhereNode whereClause) annPretty contents
