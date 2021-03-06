{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeApplications #-}

module View where

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

data DocTextTree
    = TextLeaf Text | Line | Annotated HoleAnnotation [DocTextTree]
    deriving ( Show, Eq )

makePrisms ''DocTextTree

renderSmart :: Doc HoleAnnotation -> View Action
renderSmart
    = renderTrees
    . splitMultiLineAnnotations
    . textLeavesConcat
    . textTreeForm
    . treeForm
    . layoutSmart defaultLayoutOptions

textTreeForm :: SimpleDocTree HoleAnnotation -> [DocTextTree]
textTreeForm
    = \case
        STEmpty -> one $ TextLeaf ""
        STChar c -> one . TextLeaf $ one c
        STText _ t -> one $ TextLeaf t
        STLine w -> Line : [ TextLeaf . toText $ replicate w ' ' | w > 0 ]
        STAnn ann content -> one . Annotated ann $ textTreeForm content
        STConcat contents -> concatMap textTreeForm contents

textLeavesConcat :: [DocTextTree] -> [DocTextTree]
textLeavesConcat
    = over
        (mapped % _Annotated % _2)
        (textLeavesConcat . concatByPrism _TextLeaf)

splitMultiLineAnnotations :: [DocTextTree] -> [DocTextTree]
splitMultiLineAnnotations
    = foldMap
        (\case
             Annotated ann trees -> filter
                 (\tree -> has
                      (_Annotated % _2 % traversed)
                      tree -- remove empty annotations
                  || isn't _Annotated tree)
                 . intersperse Line
                 . map (Annotated ann)
                 . splitOn Line
                 $ splitMultiLineAnnotations trees
             tree -> [ tree ])

renderTrees :: [DocTextTree] -> View Action
renderTrees [tree] = renderTree tree
renderTrees trees = span [] $ map renderTree trees

renderTree :: DocTextTree -> View Action
renderTree
    = \case
        TextLeaf t -> text $ Miso.String.ms t
        Line -> br_ []
        Annotated annotation subTrees ->
            encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: HoleAnnotation -> [View Action] -> View Action
encloseInTagFor ann views
    = case ann of
        InHole -> inHole [] views
        OutOfHole -> outOfHole [] views
        Node -> node [] views

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

paddingStyle :: Attribute action
paddingStyle = style_ $ fromList [ ("padding", "4px") ]

inHoleStyles :: [Attribute action]
inHoleStyles
    = [ style_ $ fromList [ ("background-color", "hsl(48, 100%, 85%)") ]
      , paddingStyle
      ]

inHole :: [Attribute action] -> [View action] -> View action
inHole = span . (++ inHoleStyles)

outOfHoleStyles :: [Attribute action]
outOfHoleStyles = [ class_ "has-background-white", paddingStyle ]

outOfHole :: [Attribute action] -> [View action] -> View action
outOfHole = span . (++ outOfHoleStyles)

node :: [Attribute action] -> [View action] -> View action
node = span . (class_ "node" :)

test :: Doc HoleAnnotation
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

test2 :: Doc HoleAnnotation
test2
    = PrettyPrinting.inHole
        ("outS0"
         <> PrettyPrinting.outOfHole
             ("inS1" <> PrettyPrinting.inHole (line <> "x" <> line) <> "inE1")
         <> "outE0")
