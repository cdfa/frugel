{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Scout.Node
    ( module Scout.Node
    , module Frugel.CstrSite
    , module Scout.Operators
    , ValidInterstitialWhitespace(..)
    , Expr(..)
    , Literal(..)
    , CstrSite
    , Identifier(..)
    , Decl(Decl, DeclCstrSite)
    , Node(..)
    , WhereClause(..)
    , AbstractionMeta(AbstractionMeta)
    , ExprMeta(ExprMeta)
    , Meta(Meta)
    , EvaluationStatus(..)
    , ReifiedFunction
    , ScopedEvaluation
    , ShadowingEnv
    , EvaluationRef
    , EvaluationOutput(EvaluationOutput)
    , FocusedNodeEvaluation(FocusedNodeEvaluation)
    , EvaluationError(..)
    , TypeError(..)
    , ExpectedType(..)
    , _Identifier
    , _Abstraction
    , _Application
    , _IfExpression
    , _UnaryOperation
    , _BinaryOperation
    , _Variable
    , _Literal
    , _Decl
    , _DeclCstrSite
    , _DeclNode
    , _ExprCstrSite
    , _ExprNode
    , _WhereClause
    , _WhereCstrSite
    , _WhereNode
    , _Evaluated
    , _EvaluationDeferred
    , _Elided
    , _OutOfFuel
    , _FocusedNodeEvaluation
    , exprMeta
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
import Data.Has
import Data.Hidden
import Data.Sequence       ( spanl, spanr )
import Data.String.Interpolation

import Frugel.CstrSite

import Optics.Extra.Scout  as Optics

import qualified Relude.Unsafe as Unsafe

import Scout.Internal.Node as Node
import Scout.Operators

identifier' :: String -> Maybe Identifier
identifier'
    = Identifier
    <.> (nonEmpty
         <=< traverse fromChar
         <=< guarded (not . all isDigit . preview _head))

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

ifExpression' :: Expr -> Expr -> Expr -> Expr
ifExpression' = IfExpression $ defaultExprMeta 5

unaryOperation' :: UnaryOperator -> Expr -> Expr
unaryOperation' = UnaryOperation $ defaultExprMeta 0

binaryOperation' :: Expr -> BinaryOperator -> Expr -> Expr
binaryOperation' = BinaryOperation $ defaultExprMeta 2

binaryOperation'' :: BinaryOperator -> ExprMeta -> Expr -> Expr -> Expr
binaryOperation'' binOp meta' left = BinaryOperation meta' left binOp

literal' :: Literal -> Expr
literal' = Literal $ defaultExprMeta 0

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

splitNumericSuffix :: Identifier -> (Identifier, [Char])
splitNumericSuffix identifier
    = swap
    $ (_Identifier % _UnNonEmpty)
    `passthrough` (swap
                   . second (map unAlphanumeric)
                   . spanEnd (isDigit . unAlphanumeric))
    $ identifier

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

elide :: Has Meta n => n -> n
elide = hasLens @Meta % #elided .~ True

elideExpr :: Expr -> Expr
elideExpr e
    = exprCstrSite' (fromList [])
    & elide
    & hasLens @ExprMeta % #evaluationStatus %~ \status -> case status of
        EvaluationDeferred _ -> status
        _ -> Elided (Hidden e)

deferEvaluation :: EvaluationRef Expr -> Expr
deferEvaluation eval
    = exprCstrSite' (fromList [])
    & hasLens @ExprMeta % #evaluationStatus
    .~ EvaluationDeferred (Hidden (eval, id))

whereClauseBindees :: WhereClause -> [Identifier]
whereClauseBindees = toListOf $ _WhereClause % _2 % folded % #name

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

parensInsertTest :: Expr
parensInsertTest
    = application'
        (application' (unsafeVariable "n")
                      (unsafeAbstraction "x" $ unsafeVariable "x"))
        (application'
             (unsafeVariable "y")
             (binaryOperation' (unsafeVariable "z") Plus (unsafeVariable "w")))

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
                                 succ = \n = \f = \x = f (n f x)
                                 pred = \n = \f = \x = n (\g = \h = h (g f)) (\u = x) (\u = u)
                                 mul = \m = \n = \f = m (n f)
                                 is0 = \n = n (\x = false) true
                                 Y = \f = (\x = f (x x)) (\x = f (x x))
                                 fact = Y (\f = \n = (is0 n) 1 (mul n (f (pred n))))
                                 fact2 = \n = (is0 n) 1 (mul n (fact2 (pred n)))
                                 fact3 = fact2|]
                 ]

nonTerminationSafetyTest :: CstrSite
nonTerminationSafetyTest
    = toCstrSite [ Left [str|(getNumber (k 1)) (getNumber evilWHNF)
                               where
                                 getNumber = \inspectMe = inspectMe false
                                 evilWHNF = \b = b (o o) 0
                                 k = \x = \y = x
                                 o = \x = x x
                                 false = \x = \y = y
                                 0 = \f = \x = x
                                 1 = \f = \x = f x|]
                 ]

ifTest :: CstrSite
ifTest = toCstrSite [ Left [str|if x then if y then y1 else y2 else x2|] ]

factorial :: CstrSite
factorial
    = toCstrSite [ Left [str|factorial 3
                               where
                                 factorial = \n = if n <= 1
                                                  then n
                                                  else n * factorial (n - 1)|]
                 ]
