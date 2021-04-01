{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frugel.Identifier where

import           Prettyprinter

newtype Identifier = Identifier Text
    deriving ( Eq, Ord, Show, ToString, IsString, Pretty )
