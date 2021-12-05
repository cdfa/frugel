{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Decomposition
    ( Decomposition
    , Decomposable(..)
    , ModificationStatus(..)
    , _Success
    , _Todo
    , initialDecompositionState
    , textLength
    , decompose
    , traverseNodeAt
    , traverseChildNodeAt
    , runDecomposition
    ) where

import Control.Monad.Except

import Frugel.CstrSite
import Frugel.Error.InternalError
import Frugel.Internal.DecompositionState

import Optics.Extra.Frugel

class NodeOf n ~ NodeOf (NodeOf n) => Decomposable n where
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
    -- Preserves node when cursor is at start or end. Primarily useful for nodes starting or ending with a character (e.g. lambda and parenthesis).
    conservativelyDecompose :: Int -> n -> Maybe (Int, ACstrSite (NodeOf n))
    default conservativelyDecompose
        :: IsNode n => Int -> n -> Maybe (Int, ACstrSite (NodeOf n))
    conservativelyDecompose _ n
        | hasn't (_NodeCstrSite % _CstrSite % folded) n = Nothing
    conservativelyDecompose cstrSiteOffset n = case cstrSiteOffset of
        0 -> Just (0, singletonCstrSite)
        l | l == length (toList $ decompose n) -> Just (1, singletonCstrSite)
        _ -> Nothing
      where
        singletonCstrSite = fromList [ Right $ review nodePrism n ]

instance (Decomposable n, IsNode n, n ~ NodeOf n)
    => Decomposable (ACstrSite n) where
    conservativelyDecompose _ _ = Nothing
    traverseComponents traverseChar' traverseNode'
        = traverseOf (_CstrSite % traversed)
        $ bitraverse traverseChar' traverseNode'

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

traverseNodeAt :: forall m p.
    (MonadError (InternalError p) m, Decomposable p, CstrSiteNode p)
    => (Int -> ACstrSite (NodeOf p) -> m (ACstrSite (NodeOf p)))
    -> Int
    -> p
    -> m p
traverseNodeAt f cursorOffset program
    = runDecomposition cursorOffset
                       (traverseNode @CstrSiteNode transform program)
  where
    transform :: (Decomposable n, CstrSiteNode n, NodeOf n ~ NodeOf p)
        => n
        -> Decomposition m n
    transform n = do
        cstrSiteOffset <- use #cstrSiteOffset
        lift
            $ flip setCstrSite n
            <$> case conservativelyDecompose cstrSiteOffset n of
                Just (cstrSiteOffset', cstrSite) -> f cstrSiteOffset' cstrSite
                    `catchError` const (f cstrSiteOffset $ decompose n)
                _ -> f cstrSiteOffset $ decompose n

traverseChildNodeAt :: forall m p.
    (MonadError (InternalError p) m, Decomposable p)
    => (forall n. (NodeOf n ~ NodeOf p, IsNode n) => Int -> n -> m n)
    -> Int
    -> p
    -> m p
traverseChildNodeAt f cursorOffset program
    = runDecomposition cursorOffset
                       (traverseComponents traverseChar
                                           (traverseNode @IsNode transform)
                                           program)
  where
    transform :: (NodeOf n ~ NodeOf p, IsNode n) => n -> Decomposition m n
    transform n = do
        cstrSiteOffset <- use #cstrSiteOffset
        lift $ f cstrSiteOffset n

runDecomposition :: MonadError (InternalError p) m
    => Int
    -> StateT DecompositionState m b
    -> m b
runDecomposition cursorOffset decomposition
    = runStateT decomposition (initialDecompositionState cursorOffset)
    >>= \(x, finalState) ->
    if | view #textOffset finalState
           > 0 -> throwError $ DecompositionFailed cursorOffset
       | Todo <- view #modificationStatus finalState ->
           throwError $ ASTModificationNotPerformed cursorOffset
       | otherwise -> pure x

traverseChar :: MonadState DecompositionState f => a -> f a
traverseChar c = c <$ step -- trace (show c) step

traverseNode :: forall (c :: Type
                        -> Constraint) m e n.
    ( MonadError e m
    , MonadState DecompositionState m
    , Decomposable n
    , c n
    , forall n'.
      IsNode n'
      => c n'
    )
    => (forall n'. (Decomposable n', c n', NodeOf n' ~ NodeOf n) => n' -> m n')
    -> n
    -> m n
traverseNode f n
    =
    -- do
    -- t <- guse #textOffset
    -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
    ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
    $ do
        #cstrSiteOffset
            += 1 -- At the moment, the construction site offset passed to `f` is immediately after a node where applying `f` failed. This is not a good default.
        withLocal #cstrSiteOffset 0 $ do
            newNode <- traverseComponents traverseChar (traverseNode @c f) n
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
                ((f n <* assign #modificationStatus Success)
                 `catchError` const (pure n))
                (pure newNode)
