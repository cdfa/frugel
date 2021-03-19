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

import           Optics

import           Internal.Meta ( defaultExprMeta, defaultMeta )
import           Internal.Node

makePrisms ''Node

type HoleContents' = [Either String [Node]]

identifier :: Text -> Expr
identifier = Identifier defaultExprMeta

abstraction :: Text -> Expr -> Expr
abstraction = Abstraction defaultExprMeta

application :: Expr -> Expr -> Expr
application = Application defaultExprMeta

sum :: Expr -> Expr -> Expr
sum = Sum defaultExprMeta

hole :: HoleContents -> Expr
hole = ExprHole defaultExprMeta

decl :: Text -> Expr -> Decl
decl = Decl defaultMeta

whereClause :: [Decl] -> WhereClause
whereClause = WhereClause defaultMeta

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
        [ Left "(((\\x=(", Right [ ExprNode $ identifier "x" ], Left "))))" ]

whereClauseTest :: HoleContents
whereClauseTest
    = toHoleContents
        [ Left "x where\n  y = "
        , Right [ ExprNode $ identifier "z" ]
        , Left "\n  u = w"
        ]

declNodeTest :: HoleContents
declNodeTest
    = toHoleContents
        [ Left "x where "
        , Right
              [ DeclNode $ decl "y" $ identifier "z" -- , whereClause = []
              ]
        ]

sumTest :: HoleContents
sumTest = toHoleContents [ Right [ ExprNode $ identifier "x" ], Left "+ y x" ]
