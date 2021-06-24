module Frugel.PrettyPrinting where

import Frugel.Parsing

class PrettyPrint p where
    prettyPrint :: p -> (p, [ParseErrorOf p])
