{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Optics.Extra where

import           Data.Has

import           Optics.External

concatByPrism :: (Is k An_AffineFold, Is k A_Review, Monoid a)
    => Optic' k is s a
    -> [s]
    -> [s]
concatByPrism p = concatBy (preview p) (review p)

(+~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l +~ n = over l (+ n)

(-~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l -~ n = over l (subtract n)

(+=) :: (MonadState s m, Num a, Is k A_Setter)
    => Optic k is s s a a
    -> a
    -> m ()
l += b = modify (l +~ b)

(-=) :: (MonadState s m, Num a, Is k A_Setter)
    => Optic k is s s a a
    -> a
    -> m ()
l -= b = modify (l -~ b)

hasLens :: Has a s => Lens' s a
hasLens = lens getter (\t b -> modifier (const b) t)

infixl 4 <<$>

(<<$>) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<<$>) a ffb = (a <$) <$> ffb

infixr 9 <.>

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2

-- >>> insertAt 1 99 []
-- Nothing
-- insertAt :: (Cons s s a a, Num n, Ord n) => n -> a -> s -> Maybe s
-- insertAt i x = failover (_drop i) (x <|)
-- _drop :: (Num t, Cons s s a a, Ord t) => t -> AffineTraversal' s s
-- _drop n
--     | n <= 0 = castOptic simple
-- _drop n = _tail % _drop (n - 1)
both :: Bitraversable r => Traversal (r a a) (r b b) a b
both = traversalVL $ \f -> bitraverse f f

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k = not . isn't k

-- anySucceeding :: (Foldable t, Is k An_AffineFold)
--     => t (Optic' k js a a)
--     -> AffineFold a a
-- anySucceeding = foldl' afailing $ castOptic simple
-- From https://hackage.haskell.org/package/optics-core-0.4/docs/src/Optics.Traversal.html#adjoin
-- | Combine two disjoint traversals into one.
--
-- >>> over (_1 % _Just `adjoin` _2 % _Right) not (Just True, Right False)
-- (Just False,Right True)
--
-- /Note:/ if the argument traversals are not disjoint, the result will not
-- respect the 'Traversal' laws, because it will visit the same element multiple
-- times.  See section 7 of
-- <https://www.cs.ox.ac.uk/jeremy.gibbons/publications/uitbaf.pdf Understanding Idiomatic Traversals Backwards and Forwards>
-- by Bird et al. for why this is illegal.
--
-- >>> view (partsOf (each `adjoin` _1)) ('x','y')
-- "xyx"
-- >>> set (partsOf (each `adjoin` _1)) "abc" ('x','y')
-- ('c','b')
--
-- For the 'Fold' version see 'Optics.Fold.summing'.
--
-- @since 0.4
--
adjoin :: (Is k A_Traversal, Is l A_Traversal)
    => Optic' k is s a
    -> Optic' l js s a
    -> Traversal' s a
adjoin o1 o2 = combined % traversed
  where
    combined = traversalVL $ \f s0 ->
        (\r1 r2 -> let
             s1 = evalState (traverseOf o1 update s0) r1
             s2 = evalState (traverseOf o2 update s1) r2
             in
                 s2) <$> f (toListOf (castOptic @A_Traversal o1) s0)
        <*> f (toListOf (castOptic @A_Traversal o2) s0)
    update a = get >>= \case
        a' : as' -> put as' >> pure a'
        [] -> pure a

infixr 6 `adjoin` -- Same as (<>)

{-# INLINE [1] adjoin #-}
