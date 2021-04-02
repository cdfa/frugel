{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Frugel.Layoutable where

import           Frugel.Internal.Meta    ( Meta(interstitialWhitespace) )
import           Frugel.Internal.Node    ( Decl(meta, name, value) )
import           Frugel.Internal.Program as Program
                 ( Program(meta, expr, whereClause) )
import           Frugel.Node
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Optics

class Layoutable a where
    layoutDoc :: a -> Doc Annotation

instance Layoutable CstrMaterials where
    layoutDoc = prettyCstrMaterials layoutDoc

instance Layoutable Node where
    layoutDoc (IdentifierNode name) = pretty name
    layoutDoc (ExprNode expr) = layoutDoc expr
    layoutDoc (DeclNode decl) = layoutDoc decl
    layoutDoc (WhereNode w) = layoutDoc w

instance Layoutable Expr where
    layoutDoc = parenthesizeExpr layoutDoc'
      where
        layoutDoc' (Identifier _ n) = pretty n
        layoutDoc' (Abstraction meta arg expr)
            = hcat
            $ intersperseWhitespace'
                (meta ^. #standardMeta % #interstitialWhitespace)
                [ backslash, pretty arg, equals, layoutDoc expr ]
        layoutDoc' (Application meta function arg)
            = hcat
            $ intersperseWhitespace'
                (meta ^. #standardMeta % #interstitialWhitespace)
                [ layoutDoc function, layoutDoc arg ]
        layoutDoc' (Sum meta left right)
            = hcat
            $ intersperseWhitespace'
                (meta ^. #standardMeta % #interstitialWhitespace)
                [ layoutDoc left, pretty '+', layoutDoc right ]
        layoutDoc' (ExprCstrSite _ contents) = layoutDoc contents

instance Layoutable Decl where
    layoutDoc Decl{..}
        = hcat
        $ intersperseWhitespace'
            (interstitialWhitespace meta)

            [ pretty name, pretty '=', layoutDoc value ]
    layoutDoc (DeclCstrSite _ materials) = layoutDoc materials

instance Layoutable WhereClause where
    layoutDoc (WhereClause meta decls)
        = if null decls
            then mempty
            else hcat
                $ intersperseWhitespace'
                    (interstitialWhitespace meta)
                    (pretty @Text "where" : map layoutDoc decls)
    layoutDoc (WhereCstrSite _ materials) = layoutDoc materials

instance Layoutable Program where
    layoutDoc Program{..}
        = hcat
        $ intersperseWhitespace'
            (meta ^. #standardMeta % #interstitialWhitespace)
            [ layoutDoc expr, layoutDoc whereClause ]
    layoutDoc (ProgramCstrSite _ materials) = layoutDoc materials

intersperseWhitespace' :: [Text] -> [Doc Annotation] -> [Doc Annotation]
intersperseWhitespace' = intersperseWhitespace (one . pretty)

