{-# LANGUAGE FlexibleContexts #-}

module Optics.Writer where

import Control.Monad.Writer.Class

import Optics

import Prelude              hiding ( pass )

tellFragment :: (Is k A_Setter, MonadWriter t m, Monoid s)
    => Optic k is s t a b
    -> b
    -> m ()
tellFragment l b = tell $ set l b mempty

{-# INLINE tellFragment #-}

writerFragment :: (Is k A_Setter, MonadWriter t m, Monoid s)
    => Optic k is s t a b
    -> (q, b)
    -> m q
writerFragment setter (q, output) = writer (q, mempty & setter .~ output)

writerFragment' :: (Is k A_Setter, MonadWriter t m, Monoid s)
    => Optic k is s t a b
    -> q
    -> b
    -> m q
writerFragment' setter = curry $ writerFragment setter

listening
    :: (Is k A_Getter, MonadWriter w m) => Optic' k is w u -> m a -> m (a, u)
listening l = listens $ view l

{-# INLINE listening #-}

censoring :: (Is k A_Setter, MonadWriter w m)
    => Optic k is w w u v
    -> (u -> v)
    -> m a
    -> m a
censoring l uv = censor (over l uv)

{-# INLINE censoring #-}