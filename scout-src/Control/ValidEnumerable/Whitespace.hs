module Control.ValidEnumerable.Whitespace where

import Control.Enumerable.Combinators
import Control.Sized
import Control.ValidEnumerable.Class

newtype Whitespace = Whitespace { unWhitespace :: Char }
    deriving ( Show )

instance ValidEnumerable Whitespace where
    enumerateValid = share . pay . fmap Whitespace $ elements " \t\n\r\f\v"

enumerateWhitespace :: (Sized f, Typeable f) => Shareable f Text
enumerateWhitespace = toText . map unWhitespace <$> accessValid
