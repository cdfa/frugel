module Frugel.Error
    ( module Frugel.Error
    , module Frugel.Error.InternalError
    ) where

import Frugel.Error.InternalError
import Frugel.Parsing

data Error p = ParseError (ParseErrorOf p) | InternalError (InternalError p)
