module Data.Validity.Extra where

import Data.Char
import Data.GenValidity

validateWhitespace :: ToString a => a -> Validation
validateWhitespace
    = declare "The whitespace fragment contains only whitespace"
    . all isSpace
    . toString
