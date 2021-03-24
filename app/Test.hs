module Test where

import           Prettyprinter
import           PrettyPrinting

test :: Doc Annotation
test
    = let
        prettyType = align . sep . zipWith (<+>) ("::" : repeat "->")
        prettySig name ty = pretty name <+> prettyType ty
        in
            PrettyPrinting.annotateInConstruction
            $ prettySig
                ("ex   ample" :: Text)
                [ PrettyPrinting.annotateComplete "Int"
                , "Bool"
                , "Char"
                , "IO ()"
                ]

test2 :: Doc Annotation
test2
    = PrettyPrinting.annotateInConstruction
        ("outS0"
         <> PrettyPrinting.annotateComplete
             ("inS1"
              <> PrettyPrinting.annotateInConstruction (line <> "x" <> line)
              <> "inE1")
         <> "outE0")
