{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE RecordWildCards #-}

module Frugel.Decomposition
    ( module Frugel.Decomposition
    , module Frugel.Internal.DecompositionState
    ) where

import           Frugel.Internal.DecompositionState
                 ( DecompositionState(DecompositionState) )
import           Frugel.Internal.DecompositionState
                 hiding ( DecompositionState(..) )
import           Frugel.Internal.Meta
                 ( ExprMeta(standardMeta), Meta(interstitialWhitespace) )
import           Frugel.Internal.Node               ( meta, name, value )
import           Frugel.Node

import           Optics

class Decomposable n where
    decomposed :: MonadState DecompositionState m => n -> m CstrMaterials

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    when
        (textOffset /= -1)
        (#textOffset -= 1 >> when (textOffset /= 0) (#cstrSiteOffset += 1))

instance Decomposable CstrMaterials where
    decomposed materials
        = traverseOf _CstrMaterials (foldlM foldMaterials empty) materials
      where
        foldMaterials items item
            = ifM
                (guses #textOffset (== -1))
                (pure $ snoc items item)
                (fromRight (items `snoc` item <$ step)
                 $ second (mappend items <.> processNodeItem) item)
        processNodeItem node = do
            initialCstrSiteOffset <- use #cstrSiteOffset
            nodeMaterials <- decomposed node
            state
                (\s -> if views #textOffset (== -1) s
                     then (view _CstrMaterials nodeMaterials, s)
                     else ( one $ Right node
                          , s & #cstrSiteOffset .~ (initialCstrSiteOffset + 1)
                          ))

instance Decomposable Node where
    decomposed (ExprNode n) = decomposed n
    decomposed (DeclNode n) = decomposed n
    decomposed (WhereNode n) = decomposed n

instance Decomposable Expr where
    decomposed (Identifier _ name)
        = decomposed . CstrMaterials . fromList . map Left $ toString name
    decomposed (Abstraction meta name body)
        = decomposed
        . intersperseWhitespace (interstitialWhitespace $ standardMeta meta)
        $ fromList
            (Left '\\'
             : map Left (toString name) ++ [ Left '=', Right $ ExprNode body ])
    decomposed (Application meta function arg)
        = decomposed
        . intersperseWhitespace (interstitialWhitespace $ standardMeta meta)
        $ fromList [ Right $ ExprNode function, Right $ ExprNode arg ]
    decomposed (Sum meta left right)
        = decomposed
        . intersperseWhitespace (interstitialWhitespace $ standardMeta meta)
        $ fromList [ Right $ ExprNode left, Left '+', Right $ ExprNode right ]
    decomposed (ExprCstrSite _ materials) = decomposed materials

instance Decomposable Decl where
    decomposed Decl{..}
        = decomposed . intersperseWhitespace (interstitialWhitespace meta)
        $ fromList
            (map Left (toString name) ++ [ Left '=', Right $ ExprNode value ])
    decomposed (DeclCstrSite _ materials) = decomposed materials

instance Decomposable WhereClause where
    decomposed (WhereClause meta decls)
        = decomposed . intersperseWhitespace (interstitialWhitespace meta)
        $ fromList (map Left "where" ++ map (Right . DeclNode) decls)
    decomposed (WhereCstrSite _ materials) = decomposed materials
