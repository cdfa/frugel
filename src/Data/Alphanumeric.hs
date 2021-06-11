{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Alphanumeric where

import Control.Enumerable.Combinators
import Control.ValidEnumerable

import Data.Char
import Data.Data
import Data.GenValidity

import Optics

import Prettyprinter

import Test.QuickCheck.Gen      hiding ( growingElements )

newtype Alphanumeric = Alphanumeric { unAlphanumeric :: Char }
    deriving ( Eq, Ord, Show, Generic, Data, Pretty )

makeFieldLabelsWith noPrefixFieldLabels ''Alphanumeric

instance Validity Alphanumeric where
    validate
        = mconcat [ genericValidate
                  , declare "is an alpha-numeric character"
                    . isAlphaNum
                    . unAlphanumeric
                  ]

instance GenValid Alphanumeric where
    genValid = sized uniformValid
    shrinkValid = shrinkValidStructurallyWithoutExtraFiltering -- No filtering required, because shrinking maintains the alphanumeric invariant

instance ValidEnumerable Alphanumeric where
    enumerateValid
        = share . pay . fmap Alphanumeric . growingElements
        $ [ '0' .. '9' ] ++ [ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ]

fromChar :: Char -> Maybe Alphanumeric
fromChar = Alphanumeric <.> guarded isAlphaNum
