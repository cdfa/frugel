{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- Copied from https://github.com/well-typed/optics/pull/430/files#
module Optics.ReadOnly.VL
    ( GetterVL
    , getterVL
    , toGetterVL
    , FoldVL
    , foldVL
    , toFoldVL
    ) where

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Indexed as IP

import Optics.Core       hiding ( foldVL )
import qualified Optics.Core as O
import Optics.Internal.Bi
import Optics.Internal.Optic
import Optics.Internal.Utils

newtype WrappedProfunctor p f i a b
    = WrapProfunctor { unwrapProfunctor :: p a (f b) }

instance (P.Profunctor p, Functor f)
    => IP.Profunctor (WrappedProfunctor p f) where
    dimap f g (WrapProfunctor pafb) = WrapProfunctor (P.dimap f (fmap g) pafb)
    lmap f (WrapProfunctor pafb) = WrapProfunctor (P.lmap f pafb)
    rmap g (WrapProfunctor pafb) = WrapProfunctor (P.rmap (fmap g) pafb)
    {-# INLINE dimap #-}
    {-# INLINE lmap #-}
    {-# INLINE rmap #-}
    lcoerce' = IP.lmap coerce
    rcoerce' = IP.rmap coerce
    {-# INLINE lcoerce' #-}
    {-# INLINE rcoerce' #-}

instance (P.Choice p, Applicative f) => IP.Choice (WrappedProfunctor p f) where
    left' (WrapProfunctor pafb)
        = WrapProfunctor (P.rmap (either (fmap Left) (pure . Right))
                                 (P.left' pafb))
    right' (WrapProfunctor pafb)
        = WrapProfunctor (P.rmap (either (pure . Left) (fmap Right))
                                 (P.right' pafb))
    {-# INLINE left' #-}
    {-# INLINE right' #-}

instance (P.Strong p, Functor f) => IP.Strong (WrappedProfunctor p f) where
    first' (WrapProfunctor pafb)
        = let shuffle (fb, c) = (, c) <$> fb
          in WrapProfunctor (P.rmap shuffle (P.first' pafb))
    second' (WrapProfunctor pafb)
        = let shuffle (c, fb) = (c, ) <$> fb
          in WrapProfunctor (P.rmap shuffle (P.second' pafb))
    {-# INLINE first' #-}
    {-# INLINE second' #-}

instance (P.Profunctor p, Contravariant f, Functor f)
    => Bicontravariant (WrappedProfunctor p f) where
    contrabimap f g (WrapProfunctor pafb)
        = WrapProfunctor (P.dimap f (contramap g) pafb)
    contrafirst f (WrapProfunctor pafb) = WrapProfunctor (P.lmap f pafb)
    contrasecond g (WrapProfunctor pafb)
        = WrapProfunctor (P.rmap (contramap g) pafb)
    {-# INLINE contrabimap #-}
    {-# INLINE contrafirst #-}
    {-# INLINE contrasecond #-}

instance Functor f => IP.Cochoice (WrappedProfunctor (->) f) where
    unleft (WrapProfunctor f)
        = WrapProfunctor (fmap (\(Left a) -> a) . f . Left)
    unright (WrapProfunctor f)
        = WrapProfunctor (fmap (\(Right a) -> a) . f . Right)
    {-# INLINE unleft #-}
    {-# INLINE unright #-}

instance Applicative f => IP.Visiting (WrappedProfunctor (->) f) where
    visit f (WrapProfunctor afb) = WrapProfunctor (f pure afb)
    {-# INLINE visit #-}

instance Applicative f => IP.Traversing (WrappedProfunctor (->) f) where
    wander f (WrapProfunctor afb) = WrapProfunctor (f afb)
    {-# INLINE wander #-}

type GetterVL s a
    = forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- | Build a 'Getter' from the van Laarhoven representation.
getterVL :: GetterVL s a -> Getter s a
getterVL o = to (getConst #. o Const)

{-# INLINE getterVL #-}

-- | Convert a 'Getter' to the van Laarhoven representation.
toGetterVL :: Is k A_Getter => Optic' k is s a -> GetterVL s a
toGetterVL o
    = unwrapProfunctor #. getOptic (castOptic @A_Getter o) .# WrapProfunctor

{-# INLINE toGetterVL #-}

----------------------------------------
type FoldVL s a
    = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

-- | Build a 'Fold' from the van Laarhoven representation.
foldVL :: FoldVL s a -> Fold s a
foldVL o = O.foldVL $ \f -> runTraversed
    . getConst #. o (Const #. Traversed #. f)

{-# INLINE foldVL #-}

-- | Convert a 'Fold' to the van Laarhoven representation.
toFoldVL :: Is k A_Fold => Optic' k is s a -> FoldVL s a
toFoldVL o
    = unwrapProfunctor #. getOptic (castOptic @A_Fold o) .# WrapProfunctor

{-# INLINE toFoldVL #-}
