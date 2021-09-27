{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Node
    ( module Scout.Node
    , module Frugel.CstrSite
    , ValidInterstitialWhitespace(..)
    , Expr(..)
    , CstrSite
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , AbstractionMeta(AbstractionMeta)
    , ExprMeta(ExprMeta)
    , Meta(Meta)
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
    , ScopedEvaluation
    , EvaluationError(..)
    , TypeError(..)
    , ExpectedType(..)
    , exprMeta
    , declMeta
    , whereClauseMeta
    , exprCstrSite'
    , declCstrSite'
    , whereCstrSite'
    , defaultExprMeta
    , defaultMeta
    , addMeta
    , addMetaWith
    , parenthesizeExprFromMeta
    , intersperseWhitespaceTraversers
    , whitespaceFragmentTraverser
    , validateInterstitialWhitespace
    , validateInterstitialWhitespaceWith
    , hasNonEmptyInterstitialWhitespace
    , enumerateValidExprMeta
    , enumerateValidMeta
    ) where

import Control.Lens.Plated

import Data.Alphanumeric
import Data.Char
import Data.Data.Lens
import Data.Sequence       ( spanl, spanr )
import Data.String.Interpolation

import Frugel.CstrSite

import Optics.Extra.Scout

import qualified Relude.Unsafe as Unsafe

import Scout.Internal.Node as Node

identifier' :: String -> Maybe Identifier
identifier' = Identifier <.> (nonEmpty <=< traverse fromChar)

unsafeIdentifier :: String -> Identifier
unsafeIdentifier = Unsafe.fromJust . identifier'

variable' :: Identifier -> Expr
variable' = Variable $ defaultExprMeta 0

unsafeVariable :: String -> Expr
unsafeVariable = variable' . Unsafe.fromJust . identifier'

abstraction' :: Identifier -> Expr -> Expr
abstraction' = Abstraction $ defaultAbstractionMeta 3

unsafeAbstraction :: String -> Expr -> Expr
unsafeAbstraction = abstraction' . Unsafe.fromJust . identifier'

application' :: Expr -> Expr -> Expr
application' = Application $ defaultExprMeta 1

sum' :: Expr -> Expr -> Expr
sum' = Sum $ defaultExprMeta 2

decl' :: Identifier -> Expr -> Decl
decl' = Decl $ defaultMeta 2

unsafeDecl :: String -> Expr -> Decl
unsafeDecl = decl' . Unsafe.fromJust . identifier'

whereClause' :: NonEmpty Decl -> WhereClause
whereClause' decls = WhereClause (defaultMeta $ length decls) decls

defaultAbstractionMeta :: Int -> AbstractionMeta
defaultAbstractionMeta n
    = AbstractionMeta { standardExprMeta = defaultExprMeta n
                      , reified = Nothing
                      }

abstractionMeta :: AffineTraversal' Expr AbstractionMeta
abstractionMeta = _Abstraction % _1

singleExprNodeCstrSite :: Expr -> Expr
singleExprNodeCstrSite = exprCstrSite' . one . Right . ExprNode

liftNestedCstrSiteOuterWhitespace :: CstrSite -> CstrSite
liftNestedCstrSiteOuterWhitespace
    = transformOf uniplate
    $ foldMapOf (_CstrSite % folded) extractOuterWhitespace
  where
    isWhitespaceItem (Left c) = isSpace c
    isWhitespaceItem _ = False
    extractOuterWhitespace :: Either Char Node -> CstrSite
    extractOuterWhitespace item
        = CstrSite $ leadingWhitespace <> (newItem <| trailingWhitespace)
      where
        ((leadingWhitespace, trailingWhitespace), newItem)
            = first (fromMaybe mempty)
            $ passthrough
                (_Right % _NodeCstrSite % _CstrSite)
                ((\(leadingWhitespace', (trailingWhitespace', newMaterials)) ->
                  ((leadingWhitespace', trailingWhitespace'), newMaterials))
                 . second (spanr isWhitespaceItem)
                 . spanl isWhitespaceItem)
                item

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
whitespaceId = toCstrSite [ Left "\  \tx \n=x  \tn" ]

app :: CstrSite
app = [ Left 'x', Right . ExprNode $ unsafeVariable "x", Left 'x' ]

parensTest :: CstrSite
parensTest
    = toCstrSite
        [ Left "(((\\x=(", Right . ExprNode $ unsafeVariable "x", Left "))))" ]

whereClauseTest :: CstrSite
whereClauseTest
    = toCstrSite [ Left "x where\n  y = "
                 , Right . ExprNode . exprCstrSite' $ toCstrSite [ Left "z" ]
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

evalTest :: CstrSite
evalTest
    = toCstrSite [ Left [str|fact2 (succ (succ 1))
                               where
                                 i = \x = x
                                 k = \x = \y = x
                                 s = \f = \g = \x = f x (g x)
                                 o = \x = x x
                                 true = \x = \y = x
                                 false = \x = \y = y
                                 0 = \f = \x = x
                                 1 = \f = \x = f x
                                 succ = \n = \f = \x = f(n f x)
                                 pred = \n = \f = \x = n(\g = \h = h (g f)) (\u = x) (\u =u)
                                 mul = \m = \n = \f = m(n f)
                                 is0 = \n = n (\x = false) true
                                 Y = \f = (\x = f (x x))(\x = f(x x))
                                 fact = Y(\f = \n = (is0 n) 1 (mul n (f (pred n))))
                                 fact2 = \n = (is0 n) 1 (mul n (fact3 (pred n)))
                                 fact3 = \n = (is0 n) 1 (mul n (fact2 (pred n)))
                                 infiniteRecursion = infiniteRecursion|]
                 ]
