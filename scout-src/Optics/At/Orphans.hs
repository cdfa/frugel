{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Optics.At.Orphans () where

import Data.Map.Monoidal

import Optics

type instance Index (MonoidalMap k a) = k

type instance IxValue (MonoidalMap k a) = a

instance Ord k => Ixed (MonoidalMap k a)

instance Ord k => At (MonoidalMap k a) where
    at k = lensVL $ \f -> alterF f k
    {-# INLINE at #-}

-- Adapted from https://hackage.haskell.org/package/containers-0.6.5.1/docs/src/Data.IntMap.Internal.html#alterF
alterF :: (Functor f, Ord k)
    => (Maybe a -> f (Maybe a))
    -> k
    -> MonoidalMap k a
    -> f (MonoidalMap k a)
alterF f k m = (<$> f mv) $ \case
    Nothing -> maybe m (const (delete k m)) mv
    Just v' -> insert k v' m
  where
    mv = lookup k m