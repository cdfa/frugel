module Frugel.DisplayProjection where

import           Frugel.Decomposition
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Optics

class DisplayProjection a where
    displayDoc :: a -> Doc Annotation

instance DisplayProjection CstrSite where
    displayDoc = prettyCstrSite displayDoc

instance DisplayProjection Node where
    displayDoc (IdentifierNode name) = displayDoc name
    displayDoc (ExprNode expr) = displayDoc expr
    displayDoc (DeclNode decl) = displayDoc decl
    displayDoc (WhereNode w) = displayDoc w

instance DisplayProjection Identifier where
    displayDoc (IdentifierCstrSite contents) = displayDoc contents
    displayDoc identifier = decomposableDoc identifier

instance DisplayProjection Expr where
    -- Parentheses are handled here (instead of relying on decompose), because the information would be removed when the expression is a construction site
    displayDoc = either displayDoc' parenthesize . unwrapParentheses
      where
        parenthesize (leadingFragment, e, trailingFragment)
            = parens
                (pretty leadingFragment
                 <> displayDoc e
                 <> pretty trailingFragment)
        displayDoc' (ExprCstrSite _ contents) = displayDoc contents
        displayDoc' expr = decomposableDoc expr

instance DisplayProjection Decl where
    displayDoc (DeclCstrSite _ materials) = displayDoc materials
    displayDoc decl = decomposableDoc decl

instance DisplayProjection WhereClause where
    displayDoc (WhereCstrSite _ materials) = displayDoc materials
    displayDoc whereClause = decomposableDoc whereClause

instance DisplayProjection Program where
    displayDoc (ProgramCstrSite _ materials) = displayDoc materials
    displayDoc program = decomposableDoc program

decomposableDoc :: Decomposable a => a -> Doc Annotation
decomposableDoc
    = foldMap (either pretty displayDoc) . view _CstrSite . decomposed

intersperseWhitespace' :: [Text] -> [Doc Annotation] -> [Doc Annotation]
intersperseWhitespace' interstitialWhitespace
    = intersperseWhitespace (one . pretty) interstitialWhitespace . map one

