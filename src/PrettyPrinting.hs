module PrettyPrinting where

import           Prelude                                hiding ( group )
import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine

data Annotation = InHole

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)

renderNode :: SimpleDocStream Annotation -> Text
renderNode = renderSimplyDecorated id (\InHole -> "«") (\InHole -> "»")
