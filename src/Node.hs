{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE UndecidableInstances #-}

module Node where

import           Prelude                 hiding ( group, toList )
import           Text.Megaparsec
import           Prettyprinter
import           PrettyPrinting
import           GHC.Exts
import           Steal.Megaparsec.Stream
import qualified Data.Text               as Text
import           Data.Composition

data Node
    = Identifier Text
    | Abstraction Text Node
    | Application Node Node
    | Parenthesized Node
    | Hole HoleContents
    deriving ( Eq, Ord, Show )

newtype HoleContents = HoleContents (Seq (Either Char Node))
    deriving ( Eq, Ord, Show, One, Stream, IsList )

instance VisualStream HoleContents where
    showTokens Proxy
        = showTokens (Proxy @String)
        -- Convert from Text to NonEmpty Char
        . fromList -- Assumption: prettyHoleContents of a non-empty Seq results in a non-empty render
        . toList
        -- Remove surrounding «»
        . Text.tail
        . Text.init
        -- Convert from HoleContents to Text
        . renderSmart @Text
        . prettyHoleContents
        -- Convert from NonEmpty to HoleContents
        . fromList
        . toList
    tokensLength = length .: showTokens

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

-- Invariant: prettyHoleContents of a non-empty Seq results in a non-empty render
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
