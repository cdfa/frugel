module PrettyPrinting
    ( module PrettyPrinting
    , module PrettyPrinting.Rendering
    ) where

import           Prelude                  hiding ( group )
import           Prettyprinter
import           PrettyPrinting.Rendering

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)