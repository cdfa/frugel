{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Node
    ( module Frugel.Node
    , Expr(..)
    , CstrMaterials(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , _CstrMaterials
    , exprMeta
    , prettyExpr
    , prettyCstrMaterials
    , prettyNode
    , prettyDecl
    , prettyWhereClause
    ) where

import           Frugel.Internal.Node
import           Frugel.Meta

import           Optics

makePrisms ''Node

intersperseWhitespace :: [Text] -> CstrMaterials -> CstrMaterials
intersperseWhitespace whitespaceFragments decomposables
    = fromList . concat
    $ interleave
        [ map one $ toList decomposables
        , map (map Left . toString) whitespaceFragments
        ]

-- concatCstrMaterials :: [CstrMaterials] -> CstrMaterials
-- concatCstrMaterials = CstrMaterials . join . fromList . map (view _CstrMaterials)
type CstrMaterials' = [Either String [Node]]

identifier :: Text -> Expr
identifier = Identifier defaultExprMeta

abstraction :: Text -> Expr -> Expr
abstraction = Abstraction defaultExprMeta

application :: Expr -> Expr -> Expr
application = Application defaultExprMeta

sum :: Expr -> Expr -> Expr
sum = Sum defaultExprMeta

exprCstrSite :: CstrMaterials -> Expr
exprCstrSite = ExprCstrSite defaultExprMeta

decl :: Text -> Expr -> Decl
decl = Decl defaultMeta

whereClause :: [Decl] -> WhereClause
whereClause = WhereClause defaultMeta

toCstrMaterials :: CstrMaterials' -> CstrMaterials
toCstrMaterials = fromList . concatMap (either (map Left) (map Right))

minimalCstrSite :: CstrMaterials
minimalCstrSite = one . Right . ExprNode $ identifier "x"

nested :: CstrMaterials
nested = one . Right . ExprNode . exprCstrSite $ minimalCstrSite

frugelId :: CstrMaterials
frugelId = toCstrMaterials [ Left "\\x=x" ]

frugelId' :: CstrMaterials
frugelId'
    = toCstrMaterials [ Left "\\x=", Right [ ExprNode $ identifier "x" ] ]

whitespaceId :: CstrMaterials
whitespaceId = toCstrMaterials [ Left "  \t\n\\  \tx \n=x  \t\n\n" ]

app :: CstrMaterials
app = [ Left 'x', Right . ExprNode $ identifier "x", Left 'x' ]

parensTest :: CstrMaterials
parensTest
    = toCstrMaterials
        [ Left "(((\\x=(", Right [ ExprNode $ identifier "x" ], Left "))))" ]

whereClauseTest :: CstrMaterials
whereClauseTest
    = toCstrMaterials
        [ Left "x where\n  y = "
        , Right [ ExprNode $ identifier "z" ]
        , Left "\n  u = w"
        ]

declNodeTest :: CstrMaterials
declNodeTest
    = toCstrMaterials
        [ Left "x where "
        , Right
              [ DeclNode $ decl "y" $ identifier "z" -- , whereClause = []
              ]
        ]

sumTest :: CstrMaterials
sumTest = toCstrMaterials [ Right [ ExprNode $ identifier "x" ], Left "+ y x" ]
