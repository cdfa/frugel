{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Frugel.Identifier where

import           Control.ValidEnumerable

import           Data.Alphanumeric
import           Data.Data
import           Data.GenValidity

import           Optics

import           Test.QuickCheck.Gen

newtype Identifier = Identifier (NonEmpty Alphanumeric)
    deriving ( Eq, Ord, Show, Generic, Data )

makePrisms ''Identifier

instance Validity Identifier

instance GenValid Identifier where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance ValidEnumerable Identifier where
    enumerateValid = datatype [ c1 Identifier ]

fromString :: [Char] -> Maybe Identifier
fromString = Identifier <.> (nonEmpty <=< traverse fromChar)