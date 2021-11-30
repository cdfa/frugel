{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.CstrSite.ValidEnumerable where

import Control.ValidEnumerable

import Data.Composition
import Data.GenValidity
import Data.GenValidity.Sequence ()

import Frugel.CstrSite

import Optics

import Test.QuickCheck.Gen

instance Validity n => Validity (ACstrSite n)

instance (ValidEnumerable n, GenValid n) => GenValid (ACstrSite n) where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance ValidEnumerable n => ValidEnumerable (ACstrSite n) where
    enumerateValid
        = datatype [ splurge 3 $ pure $ fromList []
                   , fromList .: (<|) <$> accessValid <*> accessValid
                   ]
