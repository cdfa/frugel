{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Frugel.PrettyPrinting where

import Frugel.DisplayProjection
import Frugel.Internal.Node ( Decl(name, value) )
import Frugel.Internal.Program ( Program(expr, whereClause) )
import Frugel.Node
import Frugel.Program

class AnnotatedPretty a where
    annPretty :: a -> Doc Annotation

instance AnnotatedPretty a => AnnotatedPretty (Maybe a) where
    annPretty = maybe mempty annPretty

instance AnnotatedPretty CstrSite where
    annPretty = prettyCstrSite annPretty

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
        annPretty' (ExprCstrSite _ contents) = annPretty contents

instance AnnotatedPretty Decl where
    annPretty Decl{..}
        = annPretty name `nestingLine` equals <+> annPretty value
    annPretty (DeclCstrSite _ contents) = annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty (WhereCstrSite _ contents) = annPretty contents
    annPretty (WhereClause _ decls)
        = nest 2
               (line'
                <> "where"
                <> nest 2 (line <> vsep (map annPretty $ toList decls)))

instance AnnotatedPretty Program where
    annPretty Program{..} = annPretty expr <> annPretty whereClause
    annPretty (ProgramCstrSite _ contents) = annPretty contents
