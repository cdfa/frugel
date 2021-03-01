{-# LANGUAGE FlexibleInstances #-}

module PrettyPrinting.Rendering where

import           Prettyprinter
import           Prettyprinter.Render.Util.StackMachine

-- import           Data.Text                              hiding ( group )
data HoleAnnotation = InHole | OutOfHole

annotationStart :: IsString p => HoleAnnotation -> p
annotationStart InHole = "«"
annotationStart OutOfHole = "»"

annotationEnd :: IsString p => HoleAnnotation -> p
annotationEnd InHole = "»"
annotationEnd OutOfHole = "«"

class Render a where
    rendered :: SimpleDocStream HoleAnnotation -> a

instance Render Text where
    rendered = renderSimplyDecorated id annotationStart annotationEnd

instance Render [Char] where
    rendered = renderSimplyDecorated toString annotationStart annotationEnd

-- renderHoleAnnotation :: Render a => SimpleDocStream HoleAnnotation -> a
-- renderHoleAnnotation
--     = try (stripPrefix "«»") . try (stripSuffix "«»") . rendered
--   where
--     try f x = fromMaybe x $ f x
renderSmart :: Render a => Doc HoleAnnotation -> a
renderSmart = rendered . layoutSmart defaultLayoutOptions