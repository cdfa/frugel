{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Optics.Extra
    ( module Optics
    , module Optics.Extra
    , module Optics.State.Operators
    , module Optics.Applicative
    ) where

import Data.Has

import Optics
import Optics.Applicative
import Optics.State.Operators

infixr 4 %%~, +~, -~, %@~

infix 4 +=, -=

infixl 4 <$^>

concatByPrism :: (Is k An_AffineFold, Is k A_Review, Monoid a)
    => Optic' k is s a
    -> [s]
    -> [s]
concatByPrism p = concatBy (preview p) (review p)

(+~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l +~ n = over l (+ n)

(-~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l -~ n = over l (subtract n)

(%%~) :: (Is k A_Traversal, Applicative f)
    => Optic k is s t a b
    -> (a -> f b)
    -> s
    -> f t
(%%~) = traverseOf

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

(%@~) :: (Is k A_Setter, is `HasSingleIndex` i)
    => Optic k is s t a b
    -> (i -> a -> b)
    -> s
    -> t
(%@~) = iover

(<$^>) :: (Is k l, Is A_Getter l, l ~ Join k A_Getter)
    => (a -> b)
    -> Optic k is s t a a
    -> Optic l is s t b b
(<$^>) = omap

omap :: (Is k l, Is A_Getter l, l ~ Join k A_Getter)
    => (a -> b)
    -> Optic k is s t a a
    -> Optic l is s t b b
omap f o = o % to f

-- using :: (Is k A_Lens, Zoom m n s t) => Optic' k is t s -> (s -> (c, s)) -> n c
-- using l f = zoom l $ state f
withLocal :: (PermeableOptic k a, MonadState s m, Is k A_Setter)
    => Optic k is s s a (ViewResult k a)
    -> ViewResult k a
    -> m b
    -> m b
withLocal o x action = do
    pre' <- o <<.= x
    result <- action
    assign o pre'
    pure result

hasLens :: Has a s => Lens' s a
hasLens = lens getter (\t b -> modifier (const b) t)

-- It is possible to make this into a Lens, but then `failover` would not return Nothing for a non-matching small
-- >>> over (refracting _1 (_tail % _init)) (first (map (*10))) ([1..5],4)
-- ([1,20,30,40,5],4)
-- >>> failover (refracting _1 (_tail % _init)) (first (map (*10))) ([1],4)
-- Nothing
refracting :: (Is k An_AffineTraversal, Is l An_AffineTraversal)
    => Optic' k is s a
    -> Optic' l js a a
    -> AffineTraversal' s s
refracting big small
    = atraversal
        (\s -> set _Left s $ traverseOf big' (matching small) s)
        (\s a ->
         over big' (\m -> fromRight m (set small' m <$> matching big s)) a)
  where
    big' = castOptic @An_AffineTraversal big
    small' = castOptic @An_AffineTraversal small

-- >>> insertAt 1 99 []
-- Nothing
--
-- insertAt :: (Cons s s a a, Num n, Ord n) => n -> a -> s -> Maybe s
-- insertAt i x = failover (_drop i) (x <|)
--
-- use slicedFrom instead
-- _drop :: (Num t, Cons s s a a, Ord t) => t -> AffineTraversal' s s
-- _drop n
--     | n <= 0 = castOptic simple
-- _drop n = _tail % _drop (n - 1)
both :: Bitraversable r => Traversal (r a a) (r b b) a b
both = traversalVL $ \f -> bitraverse f f

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is k = not . isn't k

-- It might be possible to make this work for indexed optics too, but I haven't figured out how
-- anySucceeding
--     :: Is k An_AffineFold => NonEmpty (Optic' k NoIx s a) -> AffineFold s a
-- anySucceeding = foldl1' afailing . fmap castOptic
-- adjoinAll :: Is k A_Traversal => NonEmpty (Optic' k NoIx s a) -> Traversal' s a
-- adjoinAll = foldl1' adjoin . fmap castOptic
-- retraverseOf :: (Is k An_AffineTraversal, Is k A_Review, Functor f)
--     => Optic' k is s a
--     -> (s -> f s)
--     -> a
--     -> f (Either s a)
-- retraverseOf p f = matching p <.> f . review p
-- From https://hackage.haskell.org/package/optics-core-0.4/docs/src/Optics.Traversal.html#adjoin
-- | Combine two disjoint traversals into one.
-- For the 'Fold' version see 'Optics.Fold.summing'.
--
-- @since 0.4
adjoin :: (Is k A_Traversal, Is l A_Traversal)
    => Optic' k is s a
    -> Optic' l js s a
    -> Traversal' s a
adjoin o1 o2 = combined % traversed
  where
    combined = traversalVL $ \f s0 ->
        (\r1 r2 -> let s1 = evalState (traverseOf o1 update s0) r1
                       s2 = evalState (traverseOf o2 update s1) r2
             in s2) <$> f (toListOf (castOptic @A_Traversal o1) s0)
        <*> f (toListOf (castOptic @A_Traversal o2) s0)
    update a = get >>= \case
        a' : as' -> put as' >> pure a'
        [] -> pure a

infixr 6 `adjoin` -- Same as (<>)

{-# INLINE [1] adjoin #-}
