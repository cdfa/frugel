{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Node
    ( module Frugel.Node
    , Expr(..)
    , CstrMaterials(..)
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , _CstrMaterials
    , exprMeta
    ) where

import           Frugel.Internal.Node
import           Frugel.Meta

import           Optics

makePrisms ''Node

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

_CstrSiteNode :: AffineFold Node CstrMaterials
_CstrSiteNode
    = (_IdentifierNode % _IdentifierCstrSite)
    `afailing` (_ExprNode % _ExprCstrSite % _2)
    `afailing` (_DeclNode % _DeclCstrSite % _2)
    `afailing` (_WhereNode % _WhereCstrSite % _2)

intersperseWhitespace :: IsList l => (Text -> l) -> [Text] -> [l] -> l
intersperseWhitespace toItem whitespaceFragments xs
    = fromList . concat
    $ interleave [ map toList xs, map (toList . toItem) whitespaceFragments ]

parenthesizeExpr :: (a -> a) -> (Expr -> a) -> Expr -> a
parenthesizeExpr parenthesize prettyExpr x
    | x ^. exprMeta % #parenthesisLevels > 0
        = parenthesize
        $ parenthesizeExpr
            parenthesize
            prettyExpr
            (x & exprMeta % #parenthesisLevels -~ 1)
parenthesizeExpr _ prettyExpr x = prettyExpr x

-- concatCstrMaterials :: [CstrMaterials] -> CstrMaterials
-- concatCstrMaterials = CstrMaterials . join . fromList . map (view _CstrMaterials)
identifier' :: Identifier -> Expr
identifier' = IdentifierExpr defaultExprMeta

abstraction' :: Identifier -> Expr -> Expr
abstraction' = Abstraction defaultExprMeta

application' :: Expr -> Expr -> Expr
application' = Application defaultExprMeta

sum' :: Expr -> Expr -> Expr
sum' = Sum defaultExprMeta

exprCstrSite :: CstrMaterials -> Expr
exprCstrSite = ExprCstrSite defaultExprMeta

decl' :: Identifier -> Expr -> Decl
decl' = Decl defaultMeta

whereClause' :: NonEmpty Decl -> WhereClause
whereClause' = WhereClause defaultMeta

type CstrMaterials' = [Either String Node]

toCstrMaterials :: CstrMaterials' -> CstrMaterials
toCstrMaterials = fromList . concatMap (either (map Left) (one . Right))

minimalCstrSite :: CstrMaterials
minimalCstrSite = one . Right . ExprNode $ identifier' "x"

nested :: CstrMaterials
nested = one . Right . ExprNode . exprCstrSite $ minimalCstrSite

frugelId :: CstrMaterials
frugelId = toCstrMaterials [ Left "\\x=x" ]

frugelId' :: CstrMaterials
frugelId' = toCstrMaterials [ Left "\\x=", Right . ExprNode $ identifier' "x" ]

whitespaceId :: CstrMaterials
whitespaceId = toCstrMaterials [ Left "  \t\n\\  \tx \n=x  \t\n\n" ]

app :: CstrMaterials
app = [ Left 'x', Right . ExprNode $ identifier' "x", Left 'x' ]

parensTest :: CstrMaterials
parensTest
    = toCstrMaterials
        [ Left "(((\\x=(", Right . ExprNode $ identifier' "x", Left "))))" ]

whereClauseTest :: CstrMaterials
whereClauseTest
    = toCstrMaterials
        [ Left "x where\n  y = "
        , Right . ExprNode $ identifier' "z"
        , Left "\n  u = w"
        ]

declNodeTest :: CstrMaterials
declNodeTest
    = toCstrMaterials
        [ Left "x where "
        , Right . DeclNode $ decl' "y" $ identifier' "z" -- , whereClause' = []
        ]

sumTest :: CstrMaterials
sumTest = toCstrMaterials [ Right . ExprNode $ identifier' "x", Left "+ y x" ]
