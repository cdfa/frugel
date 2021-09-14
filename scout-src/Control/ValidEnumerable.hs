{-# OPTIONS_GHC -Wno-orphans #-}

module Control.ValidEnumerable
    ( module Control.ValidEnumerable.Class
    , module Control.ValidEnumerable.Access
    , module Control.Enumerable.Combinators
    ) where

import Control.Enumerable.Combinators
import Control.Sized
import Control.ValidEnumerable.Access
import Control.ValidEnumerable.Class

import Data.Alphanumeric

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
