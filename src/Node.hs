{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Node where

import           Prelude                 hiding ( group, toList )
import           Text.Megaparsec
import           Prettyprinter
import           PrettyPrinting
import           GHC.Exts
import           Steal.Megaparsec.Stream

data Node
    = Identifier Text
    | Abstraction Text Node
    | Application Node Node
    | Parenthesized Node
    | Hole HoleContents
    deriving ( Eq, Ord, Show )

newtype HoleContents = HoleContents (Seq (Either Char Node))
    deriving ( Eq, Ord, Show, One, Stream, IsList )

-- >>> testPrettyW 3 $ prettyNode (Abstraction "x" ( Identifier "x" ))
-- >>> testPrettyW 8 $ prettyNode (Application (Application (Identifier "test") $ Identifier "test") $ Identifier "test")
-- \x
--     = x
-- test
--     test
--     test
prettyNode :: Node -> Doc HoleAnnotation
prettyNode (Identifier name) = pretty name
prettyNode (Abstraction arg node)
    = (backslash <> pretty arg) `nestingLine` equals <+> prettyNode node
prettyNode (Application function arg)
    = prettyNode function `nestingLine` prettyNode arg
prettyNode (Parenthesized node) = parens $ prettyNode node
prettyNode (Hole contents) = prettyHoleContents contents

prettyHoleContents :: HoleContents -> Doc HoleAnnotation
prettyHoleContents (HoleContents contents)
    = if null $ toList contents
        then "..."
        else annotate InHole
            . foldMap
                (either pretty (foldMap (annotate OutOfHole . prettyNode)))
            . groupByEither
            $ toList contents

minimalHole :: HoleContents
minimalHole = one . Right $ Identifier "x"

nested :: HoleContents
nested = one . Right . Hole $ minimalHole

frugelId :: HoleContents
frugelId = fromList [ Left '\\', Left 'x', Left '=', Left 'x' ]

frugelId' :: HoleContents
frugelId' = fromList [ Left '\\', Left 'x', Left '=', Right $ Identifier "x" ]

app :: HoleContents
app = fromList [ Left 'x', Right $ Identifier "x", Left 'x' ]
