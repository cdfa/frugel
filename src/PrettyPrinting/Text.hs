{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinting.Text
    ( module PrettyPrinting
    , module PrettyPrinting.Text
    ) where

import           PrettyPrinting
import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine

annotationStart, annotationEnd :: IsString p => HoleAnnotation -> p
annotationStart InHole = "«"
annotationStart OutOfHole = "»"
annotationStart Node = ""

annotationEnd InHole = "»"
annotationEnd OutOfHole = "«"
annotationEnd Node = ""

class Render a where
    rendered :: SimpleDocStream HoleAnnotation -> a

instance Render Text where
    rendered = renderSimplyDecorated id annotationStart annotationEnd

instance Render [Char] where
    rendered = renderSimplyDecorated toString annotationStart annotationEnd

renderSmart :: Render a => Doc HoleAnnotation -> a
renderSmart = rendered . layoutSmart defaultLayoutOptions