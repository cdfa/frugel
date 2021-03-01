{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Node
    ( module Node
    , Expr(..)
    , HoleContents(..)
    , Decl(Decl)
    , Node(..)
    , WhereClause(..)
    , exprMeta
    , prettyExpr
    , prettyHoleContents
    , prettyNode
    , prettyDecl
    , prettyWhereClause
    ) where

import           Prelude       hiding ( group )
import           Optics

import           Internal.Meta ( defaultMeta )
import           Internal.Node

makePrisms ''Node

type HoleContents' = [Either String [Node]]

identifier :: Text -> Expr
identifier = Identifier defaultMeta

abstraction :: Text -> Expr -> Expr
abstraction = Abstraction defaultMeta

application :: Expr -> Expr -> Expr
application = Application defaultMeta

hole :: HoleContents -> Expr
hole = Hole defaultMeta

toHoleContents :: HoleContents' -> HoleContents
toHoleContents = fromList . concatMap (either (map Left) (map Right))

minimalHole :: HoleContents
minimalHole = one . Right . ExprNode $ identifier "x"

nested :: HoleContents
nested = one . Right . ExprNode . hole $ minimalHole

frugelId :: HoleContents
frugelId = toHoleContents [ Left "\\x=x" ]

frugelId' :: HoleContents
frugelId' = toHoleContents [ Left "\\x=", Right [ ExprNode $ identifier "x" ] ]

whitespaceId :: HoleContents
whitespaceId = toHoleContents [ Left "  \t\n\\  \tx \n=x  \t\n\n" ]

app :: HoleContents
app = [ Left 'x', Right . ExprNode $ identifier "x", Left 'x' ]

parensTest :: HoleContents
parensTest
    = toHoleContents
        [ Left "(\\x=(", Right [ ExprNode $ identifier "x" ], Left "))" ]

whereClauseTest :: HoleContents
whereClauseTest
    = toHoleContents
        [ Left "x where y = ", Right [ ExprNode $ identifier "z" ] ]

declNodeTest :: HoleContents
declNodeTest
    = toHoleContents
        [ Left "x wher "
        , Right
              [ DeclNode
                $ Decl { name = "y", value = identifier "z", whereClause = [] }
              ]
        ]
