module Control.ValidEnumerable.Alphanumeric where

import           Control.Enumerable.Combinators
import           Control.ValidEnumerable.Class

newtype Alphanumeric = Alphanumeric { unAlphanumeric :: Char }

instance ValidEnumerable Alphanumeric where
    enumerateValid
        = share . pay . fmap Alphanumeric . growingElements
        $ [ '0' .. '9' ] ++ [ 'a' .. 'z' ] ++ [ 'A' .. 'Z' ]
