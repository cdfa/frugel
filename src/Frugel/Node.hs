{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Node
    ( module Frugel.Node
    , module Frugel.Meta
    , IsNode(..)
    , ValidInterstitialWhitespace(..)
    , Expr(..)
    , CstrSite(..)
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , _Abstraction
    , _Application
    , _CstrSite
    , _Decl
    , _DeclCstrSite
    , _DeclNode
    , _ExprCstrSite
    , _ExprNode
    , _Identifier
    , _IdentifierCstrSite
    , _IdentifierNode
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
    ) where

import           Frugel.Internal.Node
import           Frugel.Meta

import           Optics

parenthesizeExpr :: (a -> a) -> (Expr -> a) -> Expr -> a
parenthesizeExpr parenthesize prettyExpr x
    | x ^. exprMeta % #parenthesisLevels > 0
        = parenthesize
        $ parenthesizeExpr
            parenthesize
            prettyExpr
            (x & exprMeta % #parenthesisLevels -~ 1)
parenthesizeExpr _ prettyExpr x = prettyExpr x

unwrapParentheses :: Expr -> Either Expr (Text, Expr, Text)
unwrapParentheses e
    | e ^. exprMeta % #parenthesisLevels > 0
        = Right
            ( leadingFragment
            , e
              & exprMeta % #parenthesisLevels -~ 1
              & exprMeta % #standardMeta % #interstitialWhitespace
              .~ middleWhitespaceFragments
            , trailingFragment
            )
  where
    (leadingFragment, (middleWhitespaceFragments, trailingFragment))
        = fromMaybe
            (error
                 ("Encountered incorrect number of whitespace fragments in "
                  <> show e))
        $ preview
            (exprMeta
             % #standardMeta
             % #interstitialWhitespace
             % _Cons
             % ((,) <$^> _1 <*^> _2 % _Snoc))
            e
unwrapParentheses e = Left e

-- concatCstrSite :: [CstrSite] -> CstrSite
-- concatCstrSite = CstrSite . join . fromList . map (view _CstrSite)
identifier' :: Identifier -> Expr
identifier' = Variable $ defaultExprMeta 0

abstraction' :: Identifier -> Expr -> Expr
abstraction' = Abstraction $ defaultExprMeta 3

application' :: Expr -> Expr -> Expr
application' = Application $ defaultExprMeta 1

sum' :: Expr -> Expr -> Expr
sum' = Sum $ defaultExprMeta 2

decl' :: Identifier -> Expr -> Decl
decl' = Decl $ defaultMeta 2

whereClause' :: NonEmpty Decl -> WhereClause
whereClause' decls = WhereClause (defaultMeta $ length decls) decls

type CstrSite' = [Either String Node]

toCstrSite :: CstrSite' -> CstrSite
toCstrSite = fromList . concatMap (either (map Left) (one . Right))

minimalCstrSite :: CstrSite
minimalCstrSite = one . Right . ExprNode $ identifier' "x"

nested :: CstrSite
nested = one . Right . ExprNode . exprCstrSite' $ minimalCstrSite

frugelId :: CstrSite
frugelId = toCstrSite [ Left "\\x=x" ]

frugelId' :: CstrSite
frugelId' = toCstrSite [ Left "\\x=", Right . ExprNode $ identifier' "x" ]

whitespaceId :: CstrSite
whitespaceId = toCstrSite [ Left "\\  \tx \n=x  \t\n\n" ]

app :: CstrSite
app = [ Left 'x', Right . ExprNode $ identifier' "x", Left 'x' ]

parensTest :: CstrSite
parensTest
    = toCstrSite
        [ Left "(((\\x=(", Right . ExprNode $ identifier' "x", Left "))))" ]

whereClauseTest :: CstrSite
whereClauseTest
    = toCstrSite
        [ Left "x where\n  y = "
        , Right . ExprNode $ identifier' "z"
        , Left "\n  u = w"
        ]

declNodeTest :: CstrSite
declNodeTest
    = toCstrSite
        [ Left "x where "
        , Right . DeclNode $ decl' "y" $ identifier' "z" -- , whereClause' = []
        ]

sumTest :: CstrSite
sumTest = toCstrSite [ Right . ExprNode $ identifier' "x", Left "+ y x" ]

