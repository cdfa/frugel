{-# LANGUAGE LambdaCase #-}

module View where

import           Prelude                                 hiding ( lines )
import           Miso                                    hiding ( node, view )
import qualified Miso.String
import           Prettyprinter
import           Prettyprinter.Render.Util.SimpleDocTree
import           Frugel

import           PrettyPrinting
import           View.Elements
import           View.ViewModel                          as ViewModel
import           Optics                                  hiding ( views )
import           Event

renderSmart :: Doc Annotation -> View Action
renderSmart
    = renderTrees
    . annotationTreeForm
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm
    . layoutSmart defaultLayoutOptions

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
    Annotated ann trees -> filter isEmptyAnnotation
        . intersperse LineLeaf
        . reAnnotateCstrSite ann
        . splitOn LineLeaf
        $ splitMultiLineAnnotations trees
  where
    reAnnotateCstrSite
        (PrettyPrinting.CompletionAnnotation completionStatus)
        treeLines = case treeLines of
        (firstLine :< middleLines) :> lastLine -> reannotatedFirstLine
            <| (reannotatedMiddleLines |> reannotatedLastLine)
          where
            reannotatedFirstLine = reannotate firstLineOpenness firstLine
            reannotatedMiddleLines
                = map (reannotate middleLinesOpenness) middleLines
            reannotatedLastLine = reannotate lastLineOpenness lastLine
        _ -> map (reannotate singleLineOpenness) treeLines -- length treeLines = 0 or 1
      where
        reannotate
            = Annotated . ViewModel.CompletionAnnotation completionStatus

annotationTreeForm :: [DocTextTree RenderAnnotation] -> [Line]
annotationTreeForm = map (Line . map transform) . splitOn LineLeaf
  where
    transform = \case
        TextLeaf t -> Leaf t
        LineLeaf -> error "unexpected LineLeaf"
        Annotated ann trees -> Node ann $ map transform trees

renderTrees :: [Line] -> View Action
renderTrees
    = button_ [ keyDownHandler, noButtonStyle ] -- Using a button, because only (some) elements generate events
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
