{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Alphanumeric where

import Control.Enumerable.Combinators as Enumerable
import Control.Sized
import Control.ValidEnumerable.Access
import Control.ValidEnumerable.Class

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
        = share . pay . fmap Alphanumeric . Enumerable.elements
        $ [ '0' .. '9' ] ++ [ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ]

-- Size of characters grows for unicode characters
-- because it would otherwise explode the number of possible values of any constructor taking it as an argument
-- and thus reduce the likelihood of generating other constructors to near 0.
instance ValidEnumerable Char where
    enumerateValid
        = share
        $ unAlphanumeric <$> accessValid
        <|> aconcat (map (splurge 8)
                         [ c0 ' '
                         , c0 '\n'
                         , inflation id (minBound :: Char) (pure succ)
                         ])

fromChar :: Char -> Maybe Alphanumeric
fromChar = Alphanumeric <.> guarded isAlphaNum
