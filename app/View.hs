{-# LANGUAGE DeriveDataTypeable #-}
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
import           Optics                                  hiding ( views )
import           Data.Data.Lens
import           Data.Data

data DocTextTree
    = TextLeaf Text | Line | Annotated HoleAnnotation [DocTextTree]
    deriving ( Show, Eq, Data, Typeable )

makePrisms ''DocTextTree

subTreesTraversal :: Traversal' DocTextTree [DocTextTree]
subTreesTraversal = traversalVL $ template @DocTextTree @[DocTextTree]

renderSmart :: Doc HoleAnnotation -> View Action
renderSmart
    = renderTrees
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
        STLine w -> [ Line, TextLeaf . toText $ replicate w ' ' ]
        STAnn ann content -> one . Annotated ann $ textTreeForm content
        STConcat contents -> concatMap textTreeForm contents

textLeavesConcat :: [DocTextTree] -> [DocTextTree]
textLeavesConcat
    = over
        (mapped % subTreesTraversal)
        (textLeavesConcat . concatByPrism _TextLeaf)

renderTrees :: [DocTextTree] -> View Action
renderTrees [tree] = renderTree tree
renderTrees trees = span_ [] $ map renderTree trees

renderTree :: DocTextTree -> View Action
renderTree
    = \case
        TextLeaf t -> span_ [] [ text $ Miso.String.ms t ]
        Line -> br_ []
        Annotated annotation subTrees ->
            encloseInTagFor annotation $ map renderTree subTrees

encloseInTagFor :: HoleAnnotation -> [View Action] -> View Action
encloseInTagFor ann views
    = case ann of
        InHole -> span_ [ class_ "has-background-warning" ] views
        OutOfHole -> span_ [ class_ "has-background-white" ] views
        Node -> span_ [] views

codeStyle :: Attribute action
codeStyle
    = style_
    $ fromList
        [ ("white-space", "pre")
        , ("font-family", "\"Courier New\", monospace")
        ]

test :: Doc HoleAnnotation
test
    = let
        prettyType
            = align . sep . zipWith (<+>) ("::" : repeat "->") . map node
        prettySig name ty = node (node (pretty name) <+> prettyType ty)
        in
            inHole
            $ prettySig
                ("ex   ample" :: Text)
                [ outOfHole "Int", "Bool", "Char", "IO ()" ]
