
{-# LANGUAGE TemplateHaskell #-}

module View.ViewModel where

import           Optics
import           PrettyPrinting

data HorizontalOpenness
    = HorizontalOpenness { openLeft :: Bool, openRight :: Bool }
    deriving ( Show, Eq )

data RenderAnnotation = HoleAnnotation Depth HorizontalOpenness
    deriving ( Show, Eq )

data DocTextTree ann = TextLeaf Text | Line | Annotated ann [DocTextTree ann]
    deriving ( Show, Eq )

makePrisms ''DocTextTree

singleLineOpenness, firstLineOpenness, middleLinesOpenness, lastLineOpenness
    :: HorizontalOpenness
singleLineOpenness = HorizontalOpenness { openLeft = False, openRight = False }

firstLineOpenness = HorizontalOpenness { openLeft = False, openRight = True }

middleLinesOpenness = HorizontalOpenness { openLeft = True, openRight = True }

lastLineOpenness = HorizontalOpenness { openLeft = True, openRight = False }
