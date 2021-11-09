{-# OPTIONS_GHC -Wno-orphans #-}

module Data.NonNegative.GenValidity where

import Data.GenValidity

import Test.QuickCheck

instance (Ord a, Num a) => Validity (NonNegative a) where
    validate = declare "is non negative" . (>= NonNegative 0)

instance (Num a, Ord a, Arbitrary a) => GenValid (NonNegative a) where
    genValid = arbitrary
    shrinkValid = shrink
