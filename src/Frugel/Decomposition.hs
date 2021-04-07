{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE RecordWildCards #-}

module Frugel.Decomposition
    ( module Frugel.Internal.DecompositionState
    , Decomposable(..)
    , decompose
    ) where

import           Frugel.Identifier                  ( Identifier )
import           Frugel.Internal.DecompositionState
import           Frugel.Internal.Meta
                 ( Meta(interstitialWhitespace) )
import           Frugel.Internal.Node               ( Decl(meta, name, value) )
import           Frugel.Internal.Program            as Program
                 ( Program(meta, expr, whereClause) )
import           Frugel.Node
import           Frugel.Program

import           Optics

class Decomposable n where
    decomposed :: n -> CstrMaterials

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    when
        (textOffset /= -1)
        (#textOffset -= 1 >> when (textOffset /= 0) (#cstrSiteOffset += 1))

intersperseWhitespace' :: [Text] -> CstrMaterials' -> CstrMaterials
intersperseWhitespace' whitespaceFragments
    = fromList
    . intersperseWhitespace (map Left . toString) whitespaceFragments
    . map (either (map Left) (one . Right))

decompose :: Integer -> Program -> Maybe (Integer, CstrMaterials)
decompose cursorOffset program
    = if textOffset > 0
        then Nothing
        else Just (cstrMaterialsOffset, cstrMaterials)
  where
    (cstrMaterials, DecompositionState cstrMaterialsOffset textOffset)
        = runState (decomposeCstrMaterials $ decomposed program)
        $ initialDecompositionState cursorOffset
    decomposeCstrMaterials materials
        = traverseOf _CstrMaterials (foldlM foldMaterials empty) materials
    foldMaterials items item
        = ifM
            (guses #textOffset (== -1))
            (pure $ snoc items item)
            (fromRight (items `snoc` item <$ step)
             $ second (mappend items <.> processNodeItem) item)
    processNodeItem node = do
        initialCstrSiteOffset <- use #cstrSiteOffset
        nodeMaterials <- decomposeCstrMaterials $ decomposed node
        state (\s -> if s ^. #textOffset == -1
                   then (view _CstrMaterials nodeMaterials, s)
                   else ( one $ Right node
                        , s & #cstrSiteOffset .~ (initialCstrSiteOffset + 1)
                        ))

instance Decomposable Node where
    decomposed (IdentifierNode n) = decomposed n
    decomposed (ExprNode n) = decomposed n
    decomposed (DeclNode n) = decomposed n
    decomposed (WhereNode n) = decomposed n

instance Decomposable Identifier where
    decomposed = CstrMaterials . fromList . map Left . toString

instance Decomposable Expr where
    decomposed e
        = intersperseWhitespace'
            (e ^. exprMeta % #standardMeta % #interstitialWhitespace)
        $ parenthesizeExpr parenthesize decomposed' e
      where
        parenthesize materials = Left "(" <| (materials |> Left ")")
        decomposed' (Identifier _ name) = [ Right $ IdentifierNode name ]
        decomposed' (Abstraction _ name body)
            = [ Left [ '\\' ]
              , Right $ IdentifierNode name
              , Left "="
              , Right $ ExprNode body
              ]
        decomposed' (Application _ function arg)
            = [ Right $ ExprNode function, Right $ ExprNode arg ]
        decomposed' (Sum _ left right)
            = [ Right $ ExprNode left, Left "+", Right $ ExprNode right ]
        decomposed' (ExprCstrSite _ (CstrMaterials materials))
            = map (first one) $ toList materials

instance Decomposable Decl where
    decomposed Decl{..}
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            [ Right $ IdentifierNode name, Left "=", Right $ ExprNode value ]
    decomposed (DeclCstrSite _ materials) = materials

instance Decomposable WhereClause where
    decomposed (WhereClause meta decls)
        = intersperseWhitespace'
            (interstitialWhitespace meta)
            (Left "where" : map (Right . DeclNode) (toList decls))
    decomposed (WhereCstrSite _ materials) = materials

instance Decomposable Program where
    decomposed Program{..}
        = intersperseWhitespace'
            (meta ^. #standardMeta % #interstitialWhitespace)
            (Right (ExprNode expr)
             : maybe [] (one . Right . WhereNode) whereClause)
    decomposed (ProgramCstrSite _ materials) = materials
