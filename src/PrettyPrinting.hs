module PrettyPrinting where

import           Prettyprinter

data Depth = InHole | OutOfHole
    deriving ( Show, Eq )

newtype Annotation = HoleAnnotation Depth
    deriving ( Show, Eq )

inHole, outOfHole :: Doc Annotation -> Doc Annotation
inHole = annotate $ HoleAnnotation InHole

outOfHole = annotate $ HoleAnnotation OutOfHole

-- node = annotate Node
prettyDepth :: IsString p => Depth -> p
prettyDepth InHole = "«"
prettyDepth OutOfHole = "»"

flipDepth :: Depth -> Depth
flipDepth InHole = OutOfHole
flipDepth OutOfHole = InHole

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)