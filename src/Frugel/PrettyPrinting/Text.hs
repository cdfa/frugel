{-# LANGUAGE FlexibleInstances #-}

module Frugel.PrettyPrinting.Text
    ( module Frugel.PrettyPrinting
    , module Frugel.PrettyPrinting.Text
    ) where

import           Frugel.PrettyPrinting

import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine

annotationStart, annotationEnd :: IsString p => Annotation -> p
annotationStart (CompletionAnnotation completionStatus)
    = prettyCompletionStatus completionStatus

annotationEnd (CompletionAnnotation completionStatus)
    = prettyCompletionStatus $ flipCompletionStatus completionStatus

class Render a where
    rendered :: SimpleDocStream Annotation -> a

instance Render Text where
    rendered = renderSimplyDecorated id annotationStart annotationEnd

instance Render [Char] where
    rendered = renderSimplyDecorated toString annotationStart annotationEnd

renderSmart :: Render a => Doc Annotation -> a
renderSmart = rendered . layoutSmart defaultLayoutOptions
