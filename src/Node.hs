module Node where

data Node
    = Identifier Text
    | Abstraction Text Node
    | Application Node Node
    | Parenthesized Node
    | Hole (Maybe HoleContents)
    deriving ( Eq, Ord, Show )

type HoleContents = Seq (Either Char Node)

minimalHole :: HoleContents
minimalHole = one . Right $ Identifier "x"

nested :: HoleContents
nested = one . Right . Hole $ Just minimalHole

frugelId :: HoleContents
frugelId = fromList [ Left '\\', Left 'x', Left '=', Left 'x' ]

frugelId' :: HoleContents
frugelId' = fromList [ Left '\\', Left 'x', Left '=', Right $ Identifier "x" ]

app :: HoleContents
app = fromList [ Left 'x', Right $ Identifier "x", Left 'x' ]