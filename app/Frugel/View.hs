{-# LANGUAGE LambdaCase #-}

module Frugel.View where

import           Frugel
import           Frugel.Event
import           Frugel.View.Elements
import           Frugel.View.ViewModel                   as ViewModel

import           Miso                                    hiding ( node, view )
import qualified Miso.String

import           Optics                                  hiding ( views )

import           Prelude                                 hiding ( lines )

import           Prettyprinter.Render.Util.SimpleDocTree

renderSmart :: SimpleDocStream Annotation -> View Action
renderSmart
    = renderTrees
    . annotationTreeForm
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm

insertCursor
    :: Integer -> SimpleDocStream Annotation -> SimpleDocStream Annotation
insertCursor 0 s = SAnnPush Frugel.Cursor $ SAnnPop s
insertCursor offset s = case s of
    SFail -> error "Encountered SFail in DocStream"
    SEmpty -> error
        ("offset " <> show offset <> "was out of bound for the DocStream")
    (SChar c s') -> SChar c $ insertCursor (offset - 1) s'
    (SText len txt s')
        | offset > fromIntegral len -> SText len txt
            $ insertCursor (offset - fromIntegral len) s'
    (SText _ txt s') -> insertCursor offset . foldr SChar s' $ toString txt
    (SLine nextLineIndent s')
        | offset > 1 + fromIntegral nextLineIndent -> SLine nextLineIndent
            $ insertCursor (offset - 1 - fromIntegral nextLineIndent) s'
    (SLine nextLineIndent s') -> insertCursor offset
        $ SLine
            0
            (SText nextLineIndent (toText $ replicate nextLineIndent ' ') s')
    (SAnnPush ann s') -> SAnnPush ann $ insertCursor offset s'
    (SAnnPop s') -> SAnnPop $ insertCursor offset s'

textTreeForm :: SimpleDocTree Annotation -> [DocTextTree Annotation]
textTreeForm = \case
    STEmpty -> one $ TextLeaf ""
    STChar c -> one . TextLeaf $ one c
    STText _ t -> one $ TextLeaf t
    STLine w -> LineLeaf : [ TextLeaf . toText $ replicate w ' ' | w > 0 ]
    STAnn ann content -> one . Annotated ann $ textTreeForm content
    STConcat contents -> concatMap textTreeForm contents

textLeavesConcat :: [DocTextTree ann] -> [DocTextTree ann]
textLeavesConcat
    = over
        (mapped % _Annotated % _2)
        (textLeavesConcat . concatByPrism _TextLeaf)

splitMultiLineAnnotations
    :: [DocTextTree Annotation] -> [DocTextTree RenderAnnotation]
splitMultiLineAnnotations = foldMap $ \case
    TextLeaf t -> [ TextLeaf t ]
    LineLeaf -> [ LineLeaf ]
    Annotated Frugel.Cursor _ -> [ Annotated ViewModel.Cursor [] ]
    Annotated (Frugel.CompletionAnnotation completionStatus) trees -> filter
        (not . isEmptyTree)
        . intersperse LineLeaf
        . reAnnotateTrees completionStatus
        . splitOn LineLeaf
        $ splitMultiLineAnnotations trees
  where
    reAnnotateTrees completionStatus ((firstLine :< middleLines) :> lastLine)
        = reannotatedFirstLine
        <| (reannotatedMiddleLines |> reannotatedLastLine)
      where
        reannotatedFirstLine = reannotate firstLineOpenness firstLine
        reannotatedMiddleLines
            = map (reannotate middleLinesOpenness) middleLines
        reannotatedLastLine = reannotate lastLineOpenness lastLine
        reannotate = fromAnnotation completionStatus
    reAnnotateTrees completionStatus treeLines
        = map (fromAnnotation completionStatus singleLineOpenness) treeLines -- length treeLines = 0 or 1

annotationTreeForm :: [DocTextTree RenderAnnotation] -> [Line]
annotationTreeForm = map (Line . map transform) . splitOn LineLeaf
  where
    transform = \case
        TextLeaf t -> Leaf t
        LineLeaf -> error "unexpected LineLeaf"
        Annotated ann trees -> Node ann $ map transform trees

renderTrees :: [Line] -> View Action
renderTrees
    = button_ [ keyDownHandler, noButtonStyle, id_ "code-root" ] -- Using a button, because only (some) elements generate events
    . map (div_ [] . map renderTree . view _Line)

renderTree :: AnnotationTree -> View Action
renderTree = \case
    Leaf t -> text $ Miso.String.ms t
    Node annotation subTrees ->
        encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: RenderAnnotation -> [View Action] -> View Action
encloseInTagFor ann views = case ann of
    ViewModel.CompletionAnnotation InConstruction v ->
        inConstruction v [] views
    ViewModel.CompletionAnnotation Complete v -> complete v [] views
    ViewModel.Cursor -> caret [] []