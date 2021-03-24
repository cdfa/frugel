{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wno-unused-imports #-} -- because exporting unused qualified imports

module Prelude ( module Prelude, module Relude ) where

import           Relude
                 hiding ( Sum, abs, group, init, some, toList )
import           Prettyprinter             hiding ( list )
import           Prettyprinter.Render.Text
import           Optics
import           Data.List                 ( dropWhileEnd, stripPrefix )
import           Data.Has

testPrettyW :: Int -> Doc ann -> IO String
testPrettyW w doc
    = error $ renderStrict (layoutPretty layoutOptions (unAnnotate doc))
  where
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine w 1 }

-- Copied from the Agda package
listCase :: b -> (a -> [a] -> b) -> [a] -> b
listCase n _ [] = n
listCase _ c (x : xs) = c x xs

-- Copied from the Agda package
-- | Groups a list into alternating chunks of 'Left' and 'Right' values
groupByEither :: [Either a b] -> [Either [a] [b]]
groupByEither = listCase [] (go . init)
  where
    go :: Either [a] [b] -> [Either a b] -> [Either [a] [b]]
    go acc [] = [ adjust acc ]
    -- match: next value can be tacked onto the accumulator
    go (Left acc) (Left a : abs) = go (Left $ a : acc) abs
    go (Right acc) (Right b : abs) = go (Right $ b : acc) abs
    -- mismatch: switch the accumulator to the other mode
    go acc (ab : abs) = adjust acc : go (init ab) abs
    adjust = bimap reverse reverse
    init = bimap pure pure

concatByPrism :: (Is k An_AffineFold, Is k A_Review, Monoid a)
    => Optic' k is s a
    -> [s]
    -> [s]
concatByPrism p = concatBy (preview p) (review p)

-- >>> concatBy leftToMaybe Left [Left "h", Left "i", Right 1]
-- [Left "hi",Right 1]
concatBy :: Monoid b => (a -> Maybe b) -> (b -> a) -> [a] -> [a]
concatBy _ _ [] = []
concatBy toMonoid toElement xs
    = case spanMaybe toMonoid xs of
        ([], y : ys) -> y : concatBy' ys
        (zs, ys) -> toElement (mconcat zs) : concatBy' ys
  where
    concatBy' = concatBy toMonoid toElement

-- From https://hackage.haskell.org/package/Cabal-3.4.0.0/docs/src/Distribution.Utils.Generic.html#spanMaybe
-- >>> spanMaybe leftToMaybe [Left "h", Left "i", Right 1]
-- (["h","i"],[Right 1])
spanMaybe :: (t -> Maybe a) -> [t] -> ([a], [t])
spanMaybe _ xs@[] = ([], xs)
spanMaybe p xs@(x : xs')
    = case p x of
        Just y -> let (ys, zs) = spanMaybe p xs' in (y : ys, zs)
        Nothing -> ([], xs)

-- Modified from https://hackage.haskell.org/package/hledger-lib-1.20.4/docs/src/Hledger.Utils.html#splitAtElement
-- >>> splitOn ' ' " switch   the accumulator to the other mode   "
-- ["","switch","","","the","accumulator","to","the","other","mode","","",""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x l = prefix : rest'
  where
    (prefix, rest) = break (x ==) l
    rest'
        = case rest of
            [] -> []
            e : es
                | e == x -> splitOn x es
            es -> splitOn x es

(+~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l +~ n = over l (+ n)

(-~) :: (Num a, Is k A_Setter) => Optic k is s t a a -> a -> s -> t
l -~ n = over l (subtract n)

hasLens :: Has a s => Lens' s a
hasLens = lens getter (\t b -> modifier (const b) t)

infixl 4 <<$>

(<<$>) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
(<<$>) a ffb = (a <$) <$> ffb
