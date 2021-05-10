module Frugel.Layoutable where

import           Frugel.Decomposition
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Optics

class Layoutable a where
    layoutDoc :: a -> Doc Annotation

instance Layoutable CstrSite where
    layoutDoc = prettyCstrSite layoutDoc

instance Layoutable NodeItem where
    layoutDoc (IdentifierNode name) = layoutDoc name
    layoutDoc (ExprNode expr) = layoutDoc expr
    layoutDoc (DeclNode decl) = layoutDoc decl
    layoutDoc (WhereNode w) = layoutDoc w

instance Layoutable Identifier where
    layoutDoc (IdentifierCstrSite contents) = layoutDoc contents
    layoutDoc identifier = layoutDecomposable identifier

instance Layoutable Expr where
    -- Parentheses are handled here (instead of relying on decompose), because the information would be removed when the expression is a construction site
    layoutDoc = either layoutDoc' parenthesize . unwrapParentheses
      where
        parenthesize (leadingFragment, e, trailingFragment)
            = parens
                (pretty leadingFragment
                 <> layoutDoc e
                 <> pretty trailingFragment)
        layoutDoc' (ExprCstrSite _ contents) = layoutDoc contents
        layoutDoc' expr = layoutDecomposable expr

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
    = foldMap (either pretty layoutDoc) . view _CstrSite . decomposed

intersperseWhitespace' :: [Text] -> [Doc Annotation] -> [Doc Annotation]
intersperseWhitespace' interstitialWhitespace
    = intersperseWhitespace (one . pretty) interstitialWhitespace . map one

