{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Node
    ( module Scout.Node
    , module Scout.Meta
    , module Frugel.CstrSite
    , ValidInterstitialWhitespace(..)
    , Expr(..)
    , CstrSite
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , _Identifier
    , _Abstraction
    , _Application
    , _Decl
    , _DeclCstrSite
    , _DeclNode
    , _ExprCstrSite
    , _ExprNode
    , _Sum
    , _Variable
    , _WhereClause
    , _WhereCstrSite
    , _WhereNode
    , exprMeta
    , declMeta
    , exprCstrSite'
    , declCstrSite'
    , whereCstrSite'
    , addMeta
    , addMetaWith
    , parenthesizeExpr
    , unwrapParentheses
    , intersperseWhitespaceTraversers
    , whitespaceFragmentTraverser
    ) where

import Frugel.CstrSite

import Relude.Unsafe ( fromJust )

import Scout.Internal.Node as Node
import Scout.Meta

variable' :: Identifier -> Expr
variable' = Variable $ defaultExprMeta 0

unsafeVariable :: String -> Expr
unsafeVariable = variable' . fromJust . Node.fromString

abstraction' :: Identifier -> Expr -> Expr
abstraction' = Abstraction $ defaultExprMeta 3

unsafeAbstraction :: String -> Expr -> Expr
unsafeAbstraction = abstraction' . fromJust . Node.fromString

application' :: Expr -> Expr -> Expr
application' = Application $ defaultExprMeta 1

sum' :: Expr -> Expr -> Expr
sum' = Sum $ defaultExprMeta 2

decl' :: Identifier -> Expr -> Decl
decl' = Decl $ defaultMeta 2

unsafeDecl :: String -> Expr -> Decl
unsafeDecl = decl' . fromJust . Node.fromString

whereClause' :: NonEmpty Decl -> WhereClause
whereClause' decls = WhereClause (defaultMeta $ length decls) decls

type CstrSite' = [Either String Node]

toCstrSite :: CstrSite' -> CstrSite
toCstrSite = fromList . concatMap (either (map Left) (one . Right))

minimalCstrSite :: CstrSite
minimalCstrSite = one . Right . ExprNode $ unsafeVariable "x"

nested :: CstrSite
nested = one . Right . ExprNode . exprCstrSite' $ minimalCstrSite

frugelId :: CstrSite
frugelId = toCstrSite [ Left "\\x=x" ]

frugelId' :: CstrSite
frugelId' = toCstrSite [ Left "\\x=", Right . ExprNode $ unsafeVariable "x" ]

whitespaceId :: CstrSite
whitespaceId = toCstrSite [ Left "\\  \tx \n=x  \t\n\n" ]

app :: CstrSite
app = [ Left 'x', Right . ExprNode $ unsafeVariable "x", Left 'x' ]

parensTest :: CstrSite
parensTest
    = toCstrSite
        [ Left "(((\\x=(", Right . ExprNode $ unsafeVariable "x", Left "))))" ]

whereClauseTest :: CstrSite
whereClauseTest
    = toCstrSite [ Left "x+y where\n  y = "
                 , Right . ExprNode . exprCstrSite' $ fromList [ Left 'z' ]
                 , Left "\n  u = w"
                 ]

declNodeTest :: CstrSite
declNodeTest
    = toCstrSite
        [ Left "x where "
        , Right . DeclNode $ unsafeDecl "y" $ unsafeVariable "z" -- , whereClause' = []
        ]

sumTest :: CstrSite
sumTest = toCstrSite [ Right . ExprNode $ unsafeVariable "x", Left "+ y x" ]


