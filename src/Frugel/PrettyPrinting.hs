-- Doc's are used in Model which needs to have an Eq instance, so Reactive/conditional layouts and filling function can not be used
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Frugel.PrettyPrinting
    ( module Frugel.PrettyPrinting
    , module Prettyprinter
    ) where

import           Data.Data

import           Frugel.Internal.Node    ( Decl(name, value) )
import           Frugel.Internal.Program ( Program(expr, whereClause) )
import           Frugel.Node
import           Frugel.Program

import           Prettyprinter

data CompletionStatus = InConstruction | Complete
    deriving ( Show, Eq, Data )

data Annotation
    = CompletionAnnotation CompletionStatus
    | Cursor -- Cursor is only supposed to be inserted into SimpleDocStream after layout. Any contents will be discarded.
    deriving ( Show, Eq, Data )

annotateInConstruction, annotateComplete :: Doc Annotation -> Doc Annotation
annotateInConstruction = annotate $ CompletionAnnotation InConstruction

annotateComplete = annotate $ CompletionAnnotation Complete

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = x <> nest 4 (softline <> y)

-- Invariant: prettyCstrSite of a non-empty Seq results in a non-empty render
prettyCstrSite :: (Node -> Doc Annotation) -> CstrSite -> Doc Annotation
prettyCstrSite prettyNode (CstrSite contents)
    = annotateInConstruction
    . foldMap (either pretty (foldMap (annotateComplete . prettyNode)))
    . groupByEither
    $ toList contents

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
    annPretty
        Decl{..} = annPretty name `nestingLine` equals <+> annPretty value
    annPretty (DeclCstrSite _ contents) = annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty (WhereCstrSite _ contents) = annPretty contents
    annPretty (WhereClause _ decls)
        = nest
            2
            (line'
             <> "where"
             <> nest 2 (line <> vsep (map annPretty $ toList decls)))

instance AnnotatedPretty Program where
    annPretty Program{..} = annPretty expr <> annPretty whereClause
    annPretty (ProgramCstrSite _ contents) = annPretty contents
