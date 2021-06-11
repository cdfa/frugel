{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Prelude
    ( module Prelude
    , module Relude
    , module Data.Default.Class
    , (><)
    , toList
    , dup
    ) where

import Data.Default.Class
import qualified Data.Foldable as Foldable
import Data.List    ( groupBy )
import Data.Sequence ( (><) )

import GHC.Exts

import Relude       hiding ( Sum, abs, group, toList )
import Relude.Extra.Tuple

infixl 4 <<$>

(<<$>) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<<$>) a ffb = (a <$) <$> ffb

infixr 9 <.>

(<.>) :: Functor f => (a -> b) -> (c -> f a) -> c -> f b
f1 <.> f2 = fmap f1 . f2

infixl 4 <<*>>

(<<*>>)
    :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

lift2 :: forall (s :: (* -> *)
                 -> *
                 -> *) t m a.
    (MonadTrans s, MonadTrans t, Monad (t m), Monad m)
    => m a
    -> s (t m) a
lift2 = lift . lift

-- Copied from the Agda package
listCase :: b -> (a -> [a] -> b) -> [a] -> b
listCase n _ [] = n
listCase _ c (x : xs) = c x xs

-- Copied from the Agda package
-- | Groups a list into alternating chunks of 'Left' and 'Right' values
groupByEither :: [Either a b] -> [Either [a] [b]]
groupByEither = listCase [] (go . initial)
  where
    go :: Either [a] [b] -> [Either a b] -> [Either [a] [b]]
    go acc [] = [ adjust acc ]
    -- match: next value can be tacked onto the accumulator
    go (Left acc) (Left a : abs) = go (Left $ a : acc) abs
    go (Right acc) (Right b : abs) = go (Right $ b : acc) abs
    -- mismatch: switch the accumulator to the other mode
    go acc (ab : abs) = adjust acc : go (initial ab) abs
    adjust = bimap reverse reverse
    initial = bimap pure pure

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

-- Copied from https://hackage.haskell.org/package/monad-loops-0.4.3/docs/src/Control-Monad-Loops.html#concatM
-- | Compose a list of monadic actions into one action.  Composes using
-- ('>=>') - that is, the output of each action is fed to the input of
-- the one after it in the list.
chain :: (Monad m, Foldable t) => t (a -> m a) -> a -> m a
chain = foldr (>=>) return

-- foldAlt :: (Foldable t, Alternative f) => t a -> f a
-- foldAlt = getAlt . foldMap (Alt . pure)
-- Copied from https://hackage.haskell.org/package/extra-1.7.9/docs/src/Data.List.Extra.html#groupSort
groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f
    = map (map snd)
    . groupBy ((==) `on` fst)
    . sortBy (compare `on` fst)
    . map (f &&& id)
