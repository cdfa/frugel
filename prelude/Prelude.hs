{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Prelude
    ( module Prelude
    , module Relude
    , module Control.Monad.Reader
    , (><)
    , toList
    , dup
    ) where

import Control.Monad.Reader
    ( MonadReader(..), Reader, ReaderT(ReaderT), asks, mapReader, mapReaderT
    , runReader, runReaderT, withReader, withReaderT )

import qualified Data.Foldable as Foldable
import Data.List      ( groupBy )
import Data.Sequence  ( (><) )

import GHC.Exts

import Relude
    hiding ( Sum, abs, group, newEmptyMVar, newMVar, putMVar, readMVar, swapMVar
           , takeMVar, toList, truncate, tryPutMVar, tryReadMVar, tryTakeMVar )
import Relude.Extra.Tuple

infixl 4 <<$>

(<<$>) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<<$>) a ffb = (a <$) <$> ffb

infixr 9 <.>

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2

lift2 :: forall (s :: (Type -> Type)
                 -> Type
                 -> Type) t m a.
    (MonadTrans s, MonadTrans t, Monad (t m), Monad m)
    => m a
    -> s (t m) a
lift2 = lift . lift

-- From https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Data.Function.HT.Private.html#nest
{-# INLINE nTimes #-}
nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes n f x = f (nTimes (n - 1) f x)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen condition f = chain @Maybe (f <$ guard condition)

chain :: Foldable t => t (a -> a) -> a -> a
chain = foldr (.) id

-- >>> concatBy leftToMaybe Left [Left "h", Left "i", Right 1]
-- [Left "hi",Right 1]
concatBy :: Monoid b => (a -> Maybe b) -> (b -> a) -> [a] -> [a]
concatBy _ _ [] = []
concatBy toMonoid toElement xs = case spanMaybe toMonoid xs of
    ([], y : ys) -> y : concatBy' ys
    (zs, ys) -> toElement (mconcat zs) : concatBy' ys
  where
    concatBy' = concatBy toMonoid toElement

-- From https://hackage.haskell.org/package/Cabal-3.4.0.0/docs/src/Distribution.Utils.Generic.html#spanMaybe
-- >>> spanMaybe leftToMaybe [Left "h", Left "i", Right 1]
-- (["h","i"],[Right 1])
spanMaybe :: (t -> Maybe a) -> [t] -> ([a], [t])
spanMaybe _ xs@[] = ([], xs)
spanMaybe p xs@(x : xs') = case p x of
    Just y -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
    Nothing -> ([], xs)

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p = swap . bimap reverse reverse . span p . reverse

-- Modified from https://hackage.haskell.org/package/hledger-lib-1.20.4/docs/src/Hledger.Utils.html#splitAtElement
-- >>> splitOn ' ' " switch   the accumulator to the other mode   "
-- ["","switch","","","the","accumulator","to","the","other","mode","","",""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x l = prefix : rest'
  where
    (prefix, rest) = break (x ==) l
    rest' = case rest of
        [] -> []
        e : es | e == x -> splitOn x es
        es -> splitOn x es

-- From: https://hackage.haskell.org/package/universe-base-1.1.2/docs/src/Data.Universe.Helpers.html#interleave
-- | Fair n-way interleaving: given a finite number of (possibly infinite)
-- lists, produce a single list such that whenever @v@ has finite index in one
-- of the input lists, @v@ also has finite index in the output list. No list's
-- elements occur more frequently (on average) than another's.
interleave :: [[a]] -> [a]
interleave = concat . transpose

{-# INLINE fromFoldable #-}

-- | Convert from 'Data.Foldable.Foldable' to an 'IsList' type.
fromFoldable :: (Foldable f, IsList a) => f (Item a) -> a
fromFoldable = fromList . Foldable.toList

-- foldAlt :: (Foldable t, Alternative f) => t a -> f a
-- foldAlt = getAlt . foldMap (Alt . pure)
-- Copied from https://hackage.haskell.org/package/extra-1.7.9/docs/src/Data.List.Extra.html#groupSort
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f
    = map (map snd)
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map (f &&& id)

-- Copied from https://hackage.haskell.org/package/ilist-0.4.0.1/docs/src/Data.List.Index.html#insertAt
{- |
'insertAt' inserts an element at the given position:

@
(insertAt i x xs) !! i == x
@

If the index is negative or exceeds list length, the original list will be returned. (If the index is equal to the list length, the insertion can be carried out.)
-}
insertAt :: Int -> a -> [a] -> [a]
insertAt i a ls | i < 0 = ls
                | otherwise = go i ls
  where
    go 0 xs = a : xs
    go n (x : xs) = x : go (n - 1) xs
    go _ [] = []
