{-# LANGUAGE LambdaCase #-}

module View where

import           Prelude                                 hiding ( lines )
import           Miso                                    hiding ( node )
import qualified Miso.String
import           Prettyprinter
import           Prettyprinter.Render.Util.SimpleDocTree
import           Frugel

import           PrettyPrinting
                 hiding ( inHole, node, outOfHole )
import           View.Elements
import           View.ViewModel                          as ViewModel
import           Optics                                  hiding ( views )

renderSmart :: Doc Annotation -> View Action
renderSmart
    = renderTrees
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm
    . layoutSmart defaultLayoutOptions

textTreeForm :: SimpleDocTree Annotation -> [DocTextTree Annotation]
textTreeForm
    = \case
        STEmpty -> one $ TextLeaf ""
        STChar c -> one . TextLeaf $ one c
        STText _ t -> one $ TextLeaf t
        STLine w -> Line : [ TextLeaf . toText $ replicate w ' ' | w > 0 ]
        STAnn ann content -> one . Annotated ann $ textTreeForm content
        STConcat contents -> concatMap textTreeForm contents

textLeavesConcat :: [DocTextTree ann] -> [DocTextTree ann]
textLeavesConcat
    = over
        (mapped % _Annotated % _2)
        (textLeavesConcat . concatByPrism _TextLeaf)

splitMultiLineAnnotations
    :: [DocTextTree Annotation] -> [DocTextTree RenderAnnotation]
splitMultiLineAnnotations
    = foldMap
    $ \case
        TextLeaf t -> [ TextLeaf t ]
        Line -> [ Line ]
        Annotated ann trees -> filter isEmptyAnnotation
            . intersperse Line
            . reannotateTrees ann
            . splitOn Line
            $ splitMultiLineAnnotations trees
  where
    reannotateTrees ann treeLines
        = case ann of
            PrettyPrinting.Node -> map (Annotated ViewModel.Node) treeLines
            PrettyPrinting.HoleAnnotation
                depth -> reAnnotateHole depth treeLines
    reAnnotateHole depth treeLines
        = case treeLines of
            (firstLine : middleLines) :> lastLine -> reannotatedFirstLine
                : (reannotatedMiddleLines |> reannotatedLastLine)
              where
                reannotatedFirstLine = reannotate firstLineOpenness firstLine
                reannotatedMiddleLines
                    = map (reannotate middleLinesOpenness) middleLines
                reannotatedLastLine = reannotate lastLineOpenness lastLine
            _ -> map (reannotate singleLineOpenness) treeLines -- length treeLines = 0 or 1
      where
        reannotate = Annotated . ViewModel.HoleAnnotation depth

isEmptyAnnotation :: DocTextTree ann -> Bool
isEmptyAnnotation tree
    = has (_Annotated % _2 % traversed) tree || isn't _Annotated tree

renderTrees :: [DocTextTree RenderAnnotation] -> View Action
renderTrees [tree] = renderTree tree
renderTrees trees = span [] $ map renderTree trees

renderTree :: DocTextTree RenderAnnotation -> View Action
renderTree
    = \case
        TextLeaf t -> text $ Miso.String.ms t
        Line -> br_ []
        Annotated annotation subTrees ->
            encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: RenderAnnotation -> [View Action] -> View Action
encloseInTagFor ann views
    = case ann of
        ViewModel.HoleAnnotation InHole v -> inHole v [] views
        ViewModel.HoleAnnotation OutOfHole v -> outOfHole v [] views
        ViewModel.Node -> node [] views
