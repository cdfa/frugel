{-# LANGUAGE LambdaCase #-}

module Frugel.View where

import Frugel
import Frugel.View.Elements              as Elements
import Frugel.View.ViewModel             as ViewModel

import Miso                              hiding ( node, view )
import qualified Miso.String

import Optics.Extra                      hiding ( views )

import Prelude                           hiding ( lines )

import Prettyprinter.Render.Util.SimpleDocTree

webPrint :: Miso.String.ToMisoString a => a -> View Action
webPrint x = pre_ [] [ text $ Miso.String.ms x ]

renderSmart :: SimpleDocStream Annotation -> [View Action]
renderSmart
    = renderTrees
    . annotationTreeForm
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm

insertCursor :: Int -> SimpleDocStream Annotation -> SimpleDocStream Annotation
insertCursor 0 s = SAnnPush Frugel.Cursor $ SAnnPop s
insertCursor offset s = case s of
    SFail -> error "Encountered SFail in DocStream"
    SEmpty -> error
        ("offset " <> show offset <> " was out of bounds for the DocStream")
    (SChar c s') -> SChar c $ insertCursor (offset - 1) s'
    (SText len txt s')
        | offset > len -> SText len txt $ insertCursor (offset - len) s'
    (SText _ txt s') -> insertCursor offset . foldr SChar s' $ toString txt
    (SLine nextLineIndent s')
        | offset > 1 + nextLineIndent -> SLine nextLineIndent
            $ insertCursor (offset - 1 - nextLineIndent) s'
    (SLine nextLineIndent s') -> SLine 0
        $ insertCursor (offset - 1)
        $ SText nextLineIndent (toText $ replicate nextLineIndent ' ') s'
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
    = over (mapped % _Annotated % _2)
           (textLeavesConcat . concatByPrism _TextLeaf)

splitMultiLineAnnotations
    :: [DocTextTree Annotation] -> [DocTextTree Annotation]
splitMultiLineAnnotations = foldMap $ \case
    TextLeaf t -> [ TextLeaf t ]
    LineLeaf -> [ LineLeaf ]
    Annotated Cursor _ -> [ Annotated Cursor [] ]
    Annotated (Frugel.CompletionAnnotation completionStatus) trees -> filter
        (not . isEmptyTree)
        . intersperse LineLeaf
        . reAnnotateTrees completionStatus
        . splitOn LineLeaf
        $ splitMultiLineAnnotations trees
  where
    reAnnotateTrees completionStatus ((firstLine :< middleLines) :> lastLine)
        = reannotate firstLine
        <| (map reannotate middleLines |> reannotate lastLine)
      where
        reannotate = Annotated $ CompletionAnnotation completionStatus
    reAnnotateTrees completionStatus treeLines
        = map (Annotated $ CompletionAnnotation completionStatus) treeLines -- length treeLines <= 1

annotationTreeForm :: [DocTextTree Annotation] -> [Line]
annotationTreeForm = map (Line . map transform) . splitOn LineLeaf
  where
    transform = \case
        TextLeaf t -> Leaf t
        LineLeaf -> error "unexpected LineLeaf"
        Annotated ann trees -> Node ann $ map transform trees

renderTrees :: [Line] -> [View Action]
renderTrees = map (Elements.line [] . map renderTree . view _Line)

renderTree :: AnnotationTree -> View Action
renderTree = \case
    Leaf t -> text $ Miso.String.ms t
    Node annotation subTrees ->
        encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: Annotation -> [View Action] -> View Action
encloseInTagFor ann views = case ann of
    CompletionAnnotation InConstruction -> inConstruction [] views
    CompletionAnnotation Complete -> complete [] views
    Cursor -> caret [] []
