{-# LANGUAGE RecordWildCards #-}

module Frugel.PrettyPrinting
    ( module Frugel.PrettyPrinting
    , module Prettyprinter
    ) where

import           Frugel.Internal.Node    ( Decl(name, value) )
import           Frugel.Internal.Program ( Program(expr, whereClause) )
import           Frugel.Node
import           Frugel.Program

import           Prettyprinter

data CompletionStatus = InConstruction | Complete
    deriving ( Show, Eq )

data Annotation
    = CompletionAnnotation CompletionStatus
    | Cursor -- Cursor is only supposed to be inserted into SimpleDocStream after layout. Any contents will be discarded.
    deriving ( Show, Eq )

class AnnotatedPretty a where
    annPretty :: a -> Doc Annotation

annotateInConstruction, annotateComplete :: Doc Annotation -> Doc Annotation
annotateInConstruction = annotate $ CompletionAnnotation InConstruction

annotateComplete = annotate $ CompletionAnnotation Complete

prettyCompletionStatus :: IsString p => CompletionStatus -> p
prettyCompletionStatus InConstruction = "«"
prettyCompletionStatus Complete = "»"

flipCompletionStatus :: CompletionStatus -> CompletionStatus
flipCompletionStatus InConstruction = Complete
flipCompletionStatus Complete = InConstruction

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = x <> nest 4 softline <> y

-- Invariant: prettyCstrMaterials of a non-empty Seq results in a non-empty render
prettyCstrMaterials
    :: (Node -> Doc Annotation) -> CstrMaterials -> Doc Annotation
prettyCstrMaterials prettyNode (CstrMaterials contents)
    = if null $ toList contents
        then "..."
        else annotateInConstruction
            . foldMap (either pretty (foldMap (annotateComplete . prettyNode)))
            . groupByEither
            $ toList contents

instance AnnotatedPretty CstrMaterials where
    annPretty = prettyCstrMaterials annPretty

instance AnnotatedPretty Node where
    annPretty (IdentifierNode name) = pretty name
    annPretty (ExprNode expr) = annPretty expr
    annPretty (DeclNode decl) = annPretty decl
    annPretty (WhereNode w) = annPretty w

-- >>> import           Internal.ExprMeta    ( defaultMeta )
-- >>> testPrettyW 3 . annPretty . Abstraction defaultMeta "x" $ Sum defaultMeta ( Identifier defaultMeta "x" ) ( Identifier defaultMeta "x" )
-- >>> testPrettyW 8 $ annPretty (Application defaultMeta (Application defaultMeta (Identifier defaultMeta "test") $ Identifier defaultMeta "test") $ Identifier defaultMeta "test")
-- \x
--     = x
--     + x
-- test
--     test
--     test
instance AnnotatedPretty Expr where
    annPretty = parenthesizeExpr parens annPretty'
      where
        annPretty' (Identifier _ n) = pretty n
        annPretty' (Abstraction _ arg expr)
            = (backslash <> pretty arg) `nestingLine` equals <+> annPretty expr
        annPretty' (Application _ function arg)
            = annPretty function `nestingLine` annPretty arg
        annPretty' (Sum _ left right)
            = annPretty left `nestingLine` "+" <+> annPretty right
        annPretty' (ExprCstrSite _ contents) = annPretty contents

instance AnnotatedPretty Decl where
    annPretty Decl{..} = pretty name `nestingLine` equals <+> annPretty value
    annPretty (DeclCstrSite _ contents) = annPretty contents

    -- <> annPretty whereClause
instance AnnotatedPretty WhereClause where
    annPretty (WhereCstrSite _ contents) = annPretty contents
    annPretty (WhereClause _ decls)
        = if null decls
            then mempty
            else nest
                2
                (line'
                 <> "where"
                 <> nest 2 (line <> vsep (map annPretty decls)))

instance AnnotatedPretty Program where
    annPretty Program{..} = annPretty expr <> annPretty whereClause
    annPretty (ProgramCstrSite _ contents) = annPretty contents
