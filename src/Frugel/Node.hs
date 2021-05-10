{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Node
    ( module Frugel.Node
    , module Frugel.Meta
    , Expr(..)
    , CstrSite(..)
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , _CstrSite
    , exprMeta
    ) where

import           Frugel.Internal.Node
import           Frugel.Meta

import           Optics

makePrisms ''Node

makePrisms ''Expr

makePrisms ''Decl

makePrisms ''WhereClause

_CstrSiteNode :: AffineFold Node CstrSite

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
identifier' = Variable defaultExprMeta

abstraction' :: Identifier -> Expr -> Expr
abstraction' = Abstraction defaultExprMeta

application' :: Expr -> Expr -> Expr
application' = Application defaultExprMeta

sum' :: Expr -> Expr -> Expr
sum' = Sum defaultExprMeta

exprCstrSite :: CstrSite -> Expr

exprCstrSite = ExprCstrSite defaultExprMeta

decl' :: Identifier -> Expr -> Decl
decl' = Decl defaultMeta

whereClause' :: NonEmpty Decl -> WhereClause
whereClause' = WhereClause defaultMeta

type CstrSite' = [Either String Node]

toCstrSite :: CstrSite' -> CstrSite
toCstrSite = fromList . concatMap (either (map Left) (one . Right))

minimalCstrSite :: CstrSite
minimalCstrSite = one . Right . ExprNode $ identifier' "x"

nested :: CstrSite
nested = one . Right . ExprNode . exprCstrSite $ minimalCstrSite

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

