module Frugel.Layoutable where

import           Frugel.Decomposition
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Optics

class Layoutable a where
    layoutDoc :: a -> Doc Annotation

instance Layoutable CstrMaterials where
    layoutDoc = prettyCstrMaterials layoutDoc

instance Layoutable Node where
    layoutDoc (IdentifierNode name) = layoutDoc name
    layoutDoc (ExprNode expr) = layoutDoc expr
    layoutDoc (DeclNode decl) = layoutDoc decl
    layoutDoc (WhereNode w) = layoutDoc w

instance Layoutable Identifier where
    layoutDoc (IdentifierCstrSite contents) = layoutDoc contents
    layoutDoc identifier = layoutDecomposable identifier

instance Layoutable Expr where
    layoutDoc (ExprCstrSite _ contents) = layoutDoc contents
    layoutDoc expr = layoutDecomposable expr

instance Layoutable Decl where
    layoutDoc (DeclCstrSite _ materials) = layoutDoc materials
    layoutDoc decl = layoutDecomposable decl

instance Layoutable WhereClause where
    layoutDoc (WhereCstrSite _ materials) = layoutDoc materials
    layoutDoc whereClause = layoutDecomposable whereClause

instance Layoutable Program where
    layoutDoc (ProgramCstrSite _ materials) = layoutDoc materials
    layoutDoc program = layoutDecomposable program

layoutDecomposable :: Decomposable a => a -> Doc Annotation
layoutDecomposable
    = concatWith (<>)
    . fmap (either pretty layoutDoc)
    . view _CstrMaterials
    . decomposed

intersperseWhitespace' :: [Text] -> [Doc Annotation] -> [Doc Annotation]
intersperseWhitespace' interstitialWhitespace
    = intersperseWhitespace (one . pretty) interstitialWhitespace . map one

