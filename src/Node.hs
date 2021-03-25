{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Node
    ( module Node
    , Expr(..)
    , CstrMaterials(..)
    , Decl(Decl)
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

import           Optics

import           Internal.Meta ( defaultExprMeta, defaultMeta )
import           Internal.Node

makePrisms ''Node

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
