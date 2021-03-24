module PrettyPrinting where

import           Prettyprinter

data CompletionStatus = InConstruction | Complete
    deriving ( Show, Eq )

newtype Annotation = CompletionAnnotation CompletionStatus
    deriving ( Show, Eq )

annotateInConstruction, annotateComplete :: Doc Annotation -> Doc Annotation
annotateInConstruction = annotate $ CompletionAnnotation InConstruction

annotateComplete = annotate $ CompletionAnnotation Complete

-- node = annotate Node
prettyCompletionStatus :: IsString p => CompletionStatus -> p
prettyCompletionStatus InConstruction = "«"
prettyCompletionStatus Complete = "»"

flipCompletionStatus :: CompletionStatus -> CompletionStatus
flipCompletionStatus InConstruction = Complete
flipCompletionStatus Complete = InConstruction

nestingLine :: Doc ann -> Doc ann -> Doc ann
nestingLine x y = group $ flatAlt (x <> nest 4 (line <> y)) (x <+> y)