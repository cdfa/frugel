{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinting.Text
    ( module PrettyPrinting
    , module PrettyPrinting.Text
    ) where

import           PrettyPrinting
import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine

annotationStart, annotationEnd :: IsString p => Annotation -> p
annotationStart (HoleAnnotation depth) = prettyDepth depth
annotationStart Node = ""

annotationEnd (HoleAnnotation depth) = prettyDepth $ flipDepth depth
annotationEnd Node = ""

class Render a where
    rendered :: SimpleDocStream Annotation -> a

instance Render Text where
    rendered = renderSimplyDecorated id annotationStart annotationEnd

instance Render [Char] where
    rendered = renderSimplyDecorated toString annotationStart annotationEnd

renderSmart :: Render a => Doc Annotation -> a
renderSmart = rendered . layoutSmart defaultLayoutOptions