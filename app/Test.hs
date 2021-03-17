module Test where

import           Prettyprinter
import           PrettyPrinting

test :: Doc Annotation
test
    = let
        prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
        prettySig name ty = pretty name <+> prettyType ty
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
