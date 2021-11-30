{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

makeFieldLabelsNoPrefix ''Alphanumeric

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

accessLetter :: (Sized f, Typeable f) => Shareable f Alphanumeric
accessLetter
    = pay . fmap Alphanumeric . Enumerable.elements
    $ [ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ]

fromChar :: Char -> Maybe Alphanumeric
fromChar = Alphanumeric <.> guarded isAlphaNum
