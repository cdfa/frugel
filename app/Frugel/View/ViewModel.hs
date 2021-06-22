{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}

module Frugel.View.ViewModel where

import Frugel

import Optics.Extra

data DocTextTree ann
    = TextLeaf Text | LineLeaf | Annotated ann [DocTextTree ann]
    deriving ( Show, Eq )

data AnnotationTree = Leaf Text | Node Annotation [AnnotationTree]

newtype Line = Line [AnnotationTree]

makePrisms ''DocTextTree

makePrisms ''Line

isEmptyTree :: DocTextTree Annotation -> Bool
isEmptyTree = \case
    TextLeaf "" -> True
    Annotated Cursor _ -> False
    Annotated (CompletionAnnotation InConstruction) _ -> False
    Annotated _ [] -> True
    Annotated _ trees -> all isEmptyTree trees
    _ -> False
