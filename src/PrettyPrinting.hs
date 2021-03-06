module PrettyPrinting where

import           Prettyprinter

data HoleAnnotation = InHole | OutOfHole | Node
    deriving ( Show, Eq )

inHole, outOfHole, node :: Doc HoleAnnotation -> Doc HoleAnnotation
inHole = annotate InHole

outOfHole = annotate OutOfHole

node = annotate Node

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)