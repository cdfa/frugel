{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeApplications #-}

module View where

import           Prelude                                 hiding ( lines )
import           Miso                                    hiding ( node )
import qualified Miso.String
import           Prettyprinter
import           Prettyprinter.Render.Util.SimpleDocTree
import           Frugel
import           PrettyPrinting
                 hiding ( inHole, node, outOfHole )
import qualified PrettyPrinting
                 ( inHole, node, outOfHole )
import           Optics                                  hiding ( views )

data HorizontalOpenness
    = HorizontalOpenness { openLeft :: Bool, openRight :: Bool }
    deriving ( Show, Eq )

data RenderAnnotation = Node | HoleAnnotation Depth HorizontalOpenness
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
            PrettyPrinting.Node -> map (Annotated View.Node) treeLines
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
        reannotate = Annotated . View.HoleAnnotation depth

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
        View.HoleAnnotation InHole v -> inHole v [] views
        View.HoleAnnotation OutOfHole v -> outOfHole v [] views
        View.Node -> node [] views

codeStyle :: Attribute action
codeStyle
    = style_
    $ fromList
        [ ("white-space", "pre")
        , ("font-family", "\"Courier New\", monospace")
        ]

spanStyle :: Attribute action
spanStyle = style_ $ fromList [ ("display", "inline-block") ]

span :: [Attribute action] -> [View action] -> View action
span = span_ . (spanStyle :)

paddingStyle :: HorizontalOpenness -> Attribute action
paddingStyle HorizontalOpenness{..}
    = style_
    $ fromList
        [ ( "padding"
          , Miso.String.ms
            $ unwords [ "4px", padding openRight, "4px", padding openLeft ]
          )
        ]
  where
    padding present = if present then "0px" else "4px"

inHoleStyles :: HorizontalOpenness -> [Attribute action]
inHoleStyles v
    = [ style_ $ fromList [ ("background-color", "hsl(48, 100%, 85%)") ]
      , paddingStyle v
      ]

inHole
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
inHole v = span . (++ inHoleStyles v)

outOfHoleStyles :: HorizontalOpenness -> [Attribute action]
outOfHoleStyles v = [ class_ "has-background-white", paddingStyle v ]

outOfHole
    :: HorizontalOpenness -> [Attribute action] -> [View action] -> View action
outOfHole v = span . (++ outOfHoleStyles v)

node :: [Attribute action] -> [View action] -> View action
node = span . (class_ "node" :)

test :: Doc Annotation
test
    = let
        prettyType
            = align
            . sep
            . zipWith (<+>) ("::" : repeat "->")
            . map PrettyPrinting.node
        prettySig name ty
            = PrettyPrinting.node
                (PrettyPrinting.node (pretty name) <+> prettyType ty)
        in
            PrettyPrinting.inHole
            $ prettySig
                ("ex   ample" :: Text)
                [ PrettyPrinting.outOfHole "Int", "Bool", "Char", "IO ()" ]

test2 :: Doc Annotation
test2
    = PrettyPrinting.inHole
        ("outS0"
         <> PrettyPrinting.outOfHole
             ("inS1" <> PrettyPrinting.inHole (line <> "x" <> line) <> "inE1")
         <> "outE0")
