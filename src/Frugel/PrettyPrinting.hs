{-# LANGUAGE FlexibleInstances #-}

module Frugel.PrettyPrinting where

import Frugel.CstrSite
import Frugel.DisplayProjection

class AnnotatedPretty a where
    annPretty :: a -> Doc Annotation

instance AnnotatedPretty a => AnnotatedPretty (Maybe a) where
    annPretty = maybe mempty annPretty

instance AnnotatedPretty n => AnnotatedPretty (ACstrSite n) where
    annPretty = prettyCstrSite annPretty
