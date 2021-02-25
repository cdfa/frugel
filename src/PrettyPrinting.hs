module PrettyPrinting where

import           Prelude                                hiding ( group )
import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine
import           Data.Text                              hiding ( group )

data HoleAnnotation = InHole | OutOfHole

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)

renderHoleAnnotation :: SimpleDocStream HoleAnnotation -> Text
renderHoleAnnotation
    stream = try (stripPrefix "«»") $ try (stripSuffix "«»") rendered
  where
    try f x = fromMaybe x $ f x
    rendered = renderSimplyDecorated id annotationStart annotationEnd stream
    annotationStart InHole = "«"
    annotationStart OutOfHole = "»"
    annotationEnd InHole = "»"
    annotationEnd OutOfHole = "«"
