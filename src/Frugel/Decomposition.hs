{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Decomposition
    ( module Frugel.Decomposition
    , _Success
    , _Todo
    , initialDecompositionState
    , DecompositionMonad
    , ModificationStatus(..)
    ) where

import Control.Monad.Except

import Frugel.CstrSite
import Frugel.Error.InternalError
import Frugel.Internal.DecompositionState

import Optics.Extra

class NodeOf n ~ NodeOf (NodeOf n) => Decomposable n where
    -- Preserves node when cursor is at start or end. Only useful for nodes starting or ending with a character (e.g. lambda and parenthesis).
    conservativelyDecompose :: Int -> n -> Maybe (Int, ACstrSite (NodeOf n))
    conservativelyDecompose _ _ = Nothing
    -- It would make sense for this function to have a mapKeyword :: Text -> f () and mapWhitespace :: Char -> f Char as well, but it's not yet needed
    -- traverseComponents could be generalised to a Bitraversal which might make implementation easier, but at the moment there is no library to work with them so I'm not sure
    traverseComponents :: Applicative f
        => (Char -> f Char)
        -> (forall n'.
            (Decomposable n', IsNode n', NodeOf n ~ NodeOf n')
            => n'
            -> f n')
        -> n
        -> f n

decompose :: Decomposable n => n -> ACstrSite (NodeOf n)
decompose n
    = fromList . reverse
    $ execState
        (traverseComponents (conses Left) (conses (Right . review nodePrism)) n)
        []
  where
    conses f x = x <$ modify (f x :)

textLength :: (Decomposable n, Decomposable (NodeOf n)) => n -> Int
textLength = sum . either (const 1) textLength <.> view _CstrSite . decompose

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    -- c <- guse #cstrSiteOffset
    -- traceM ("step t: " <> show textOffset <> " c: " <> show c)
    when (textOffset /= -1) (#textOffset -= 1)
    when (textOffset > 0) (#cstrSiteOffset += 1)

modifyNodeAt :: forall m' p.
    (MonadError (InternalError p) m', Decomposable p, CstrSiteNode p)
    => (Int -> ACstrSite (NodeOf p) -> m' (ACstrSite (NodeOf p)))
    -> Int
    -> p
    -> m' p
modifyNodeAt f cursorOffset program
    = runModification >>= \(newProgram, decompositionState) ->
    if | view #textOffset decompositionState
           > 0 -> throwError $ DecompositionFailed cursorOffset
       | Todo <- view #modificationStatus decompositionState ->
           throwError $ ASTModificationNotPerformed cursorOffset
       | otherwise -> pure newProgram
  where
    runModification
        = mapNode program `runStateT` initialDecompositionState cursorOffset
    mapChar c = c <$ step -- trace (show c) step
    mapNode :: (Decomposable n, CstrSiteNode n, NodeOf n ~ NodeOf p)
        => n
        -> DecompositionMonad m' n
    mapNode n = do
        -- t <- guse #textOffset
        -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
        ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
            $ do
                #cstrSiteOffset += 1 -- At the moment, the construction site offset passed to `f` is immediately after a node where applying `f` failed. This is not a good default.
                withLocal #cstrSiteOffset 0 $ do
                    newNode <- traverseComponents mapChar mapNode n
                    -- t <- guse #textOffset
                    -- c <- guse #cstrSiteOffset
                    -- traceM
                    --     ("post "
                    --      <> take 20 (show n)
                    --      <> " t: "
                    --      <> show t
                    --      <> " c: "
                    --      <> show c)
                    ifM (guses #textOffset (<= 0)
                         &&^ guses #modificationStatus (isn't _Success))
                        (catchError (setCstrSite <$> transform n
                                     ?? n <* assign #modificationStatus Success)
                         $ const (pure n))
                        (pure newNode)
    transform :: (Decomposable n, NodeOf n ~ NodeOf p)
        => n
        -> DecompositionMonad m' (ACstrSite (NodeOf n))
    transform n = do
        cstrSiteOffset <- use #cstrSiteOffset
        lift $ case conservativelyDecompose cstrSiteOffset n of
            Just (cstrSiteOffset', cstrSite) -> catchError
                (f cstrSiteOffset' cstrSite)
                $ const (f cstrSiteOffset $ decompose n)
            _ -> f cstrSiteOffset $ decompose n

conservativelyDecomposeNode
    :: (Is k A_Review, Is l An_AffineFold, Decomposable n)
    => Optic' k is (NodeOf n) n
    -> Optic' l is n (ACstrSite (NodeOf n))
    -> Int
    -> n
    -> Maybe (Int, ACstrSite (NodeOf n))
conservativelyDecomposeNode nodeReview cstrSiteFold cstrSiteOffset n
    = case cstrSiteOffset of
        0 | isn't cstrSiteFold n -> Just (0, singletonCstrSite)
        l | isn't cstrSiteFold n && l == length (toList $ decompose n) ->
              Just (1, singletonCstrSite)
        _ -> Nothing
  where
    singletonCstrSite = fromList [ Right $ review nodeReview n ]

instance (Decomposable n, IsNode n, n ~ NodeOf n)
    => Decomposable (ACstrSite n) where
    traverseComponents mapChar mapNode
        = traverseOf (_CstrSite % traversed) $ bitraverse mapChar mapNode


