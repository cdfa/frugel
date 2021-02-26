{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE UndecidableInstances #-}

module Node where

import           Prelude             hiding ( group, toList )
import           Text.Megaparsec
import           Prettyprinter
import           PrettyPrinting
import           GHC.Exts
import qualified Data.Text           as Text
import           Data.Composition
import           Lens.Micro.Platform

newtype Meta = Meta { _parenthesized :: Bool }
    deriving ( Eq, Ord, Show )

$(makeLenses ''Meta)

data Node
    = Identifier Meta Text
    | Abstraction Meta Text Node
    | Application Meta Node Node
    | Hole Meta HoleContents
    deriving ( Eq, Ord, Show )

newtype HoleContents = HoleContents (Seq (Either Char Node))
    deriving ( Eq, Ord, Show, One, Stream, IsList )

type HoleContents' = [Either String [Node]]

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

defaultMeta :: Meta
defaultMeta = Meta { _parenthesized = False }

identifier :: Text -> Node
identifier = Identifier defaultMeta

abstraction :: Text -> Node -> Node
abstraction = Abstraction defaultMeta

application :: Node -> Node -> Node
application = Application defaultMeta

hole :: HoleContents -> Node
hole = Hole defaultMeta

nodeMeta :: Lens' Node Meta
nodeMeta = lens getMeta setMeta
  where
    getMeta (Identifier meta _) = meta
    getMeta (Abstraction meta _ _) = meta
    getMeta (Application meta _ _) = meta
    getMeta (Hole meta _) = meta
    setMeta (Identifier _ text) meta = Identifier meta text
    setMeta (Abstraction _ argument body) meta = Abstraction meta argument body
    setMeta (Application _ function argument) meta
        = Application meta function argument
    setMeta (Hole _ contents) meta = Hole meta contents

-- >>> testPrettyW 3 $ prettyNode (Abstraction "x" ( Identifier "x" ))
-- >>> testPrettyW 8 $ prettyNode (Application (Application (Identifier "test") $ Identifier "test") $ Identifier "test")
-- \x
--     = x
-- test
--     test
--     test
prettyNode :: Node -> Doc HoleAnnotation
prettyNode node
    | node ^. nodeMeta . parenthesized
        = parens $ prettyNode (node & nodeMeta . parenthesized .~ False)
prettyNode (Identifier _ name) = pretty name
prettyNode (Abstraction _ arg node)
    = (backslash <> pretty arg) `nestingLine` equals <+> prettyNode node
prettyNode (Application _ function arg)
    = prettyNode function `nestingLine` prettyNode arg
prettyNode (Hole _ contents) = prettyHoleContents contents

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

toHoleContents :: HoleContents' -> HoleContents
toHoleContents = fromList . concatMap (either (map Left) (map Right))

minimalHole :: HoleContents
minimalHole = one . Right $ identifier "x"

nested :: HoleContents
nested = one . Right . hole $ minimalHole

frugelId :: HoleContents
frugelId = toHoleContents [ Left "\\x=x" ]

frugelId' :: HoleContents
frugelId' = toHoleContents [ Left "\\x=", Right [ identifier "x" ] ]

whitespaceId :: HoleContents
whitespaceId = toHoleContents [ Left "  \t\n\\  \tx \n=x  \t\n\n" ]

app :: HoleContents
app = fromList [ Left 'x', Right $ identifier "x", Left 'x' ]

parensTest :: HoleContents
parensTest
    = toHoleContents [ Left "(\\x=(", Right [ identifier "x" ], Left "))" ]
