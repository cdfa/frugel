{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Frugel.View.Rendering where

import Frugel
import Frugel.View.Elements              as Elements

import Miso                              hiding ( Node, node, view )
import qualified Miso.String

import Optics.Extra.Scout                hiding ( views )

import Prelude                           hiding ( lines )

import Prettyprinter.Render.Util.SimpleDocTree

import Scout.PrettyPrinting

data DocTextTree
    = TextLeaf Text | LineLeaf | Annotated Annotation [DocTextTree]
    deriving ( Show, Eq )

data AnnotationTree = Leaf Text | Node Annotation [AnnotationTree]
    deriving ( Show, Eq )

newtype Line = Line [AnnotationTree]
    deriving ( Show, Eq )

makePrisms ''DocTextTree

makePrisms ''Line

isEmptyTree :: DocTextTree -> Bool
isEmptyTree = \case
    TextLeaf "" -> True
    Annotated Cursor _ -> False
    Annotated (CompletionAnnotation InConstruction) _ -> False
    Annotated _ [] -> True
    Annotated _ trees -> all isEmptyTree trees
    _ -> False

renderPretty :: AnnotatedPretty a => a -> [View action]
renderPretty
    = renderDocStream
    . reAnnotateS toStandardAnnotation
    . layoutPretty defaultLayoutOptions
    . annPretty

-- instead of rendering to a SimpleDocStream and converting back to a tree with `treeForm`, `renderDoc` could be made to produce DocTextTree's directly, which would speed up rendering a bit
-- it would also be simpler (and probably faster) to intersperse additional SAnnPop's and SAnnPush's around SLine's using a renderFunction which counts annotation levels instead of what `splitMultiLineAnnotations` does
renderDocStream :: SimpleDocStream Annotation -> [View action]
renderDocStream
    = renderTrees
    . annotationTreeForm
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm

textTreeForm :: SimpleDocTree Annotation -> [DocTextTree]
textTreeForm = \case
    STEmpty -> one $ TextLeaf ""
    STChar '\n' -> one LineLeaf -- Normally, there would be no newlines in STChar, but these are explicitly inserted by renderCstrSite' to prevent insertion of extra whitespace when pretty printing construction sites which are `nest`ed
    STChar c -> one . TextLeaf $ one c
    STText _ t -> one $ TextLeaf t
    STLine w -> LineLeaf : [ TextLeaf . toText $ replicate w ' ' | w > 0 ]
    STAnn ann content -> one . Annotated ann $ textTreeForm content
    STConcat contents -> concatMap textTreeForm contents

-- for some reason eta-reduction here causes an "Unexpected lambda in case"
textLeavesConcat :: [DocTextTree] -> [DocTextTree]
textLeavesConcat trees
    = over (mapped % _Annotated % _2) textLeavesConcat
    $ concatByPrism _TextLeaf trees

splitMultiLineAnnotations :: [DocTextTree] -> [DocTextTree]
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
    tree@(Annotated Elided _) -> [ tree ]
  where
    reAnnotateTrees completionStatus ((firstLine :< middleLines) :> lastLine)
        = reannotate firstLine
        <| (map reannotate middleLines |> reannotate lastLine)
      where
        reannotate = Annotated $ CompletionAnnotation completionStatus
    reAnnotateTrees completionStatus treeLines
        = map (Annotated $ CompletionAnnotation completionStatus) treeLines -- length treeLines <= 1

annotationTreeForm :: [DocTextTree] -> [Line]
annotationTreeForm = map (Line . map transform) . splitOn LineLeaf
  where
    transform = \case
        TextLeaf t -> Leaf t
        LineLeaf -> error "unexpected LineLeaf"
        Annotated ann trees -> Node ann $ map transform trees

renderTrees :: [Line] -> [View action]
renderTrees = map (Elements.line [] . map renderTree . view _Line)

renderTree :: AnnotationTree -> View action
renderTree = \case
    Leaf t -> text $ Miso.String.ms t -- ms required on GHCJS
    Node annotation@(CompletionAnnotation InConstruction) [] ->
        renderTree $ Node annotation [ Leaf " " ] -- Ghost space instead of messing with CSS
    Node annotation subTrees ->
        encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: Annotation -> [View action] -> View action
encloseInTagFor ann views = case ann of
    CompletionAnnotation InConstruction -> inConstruction [] views
    CompletionAnnotation Complete -> complete [] views
    Cursor -> caret [] []
    Elided -> elided [] views
