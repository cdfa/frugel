{-# LANGUAGE TypeFamilies #-}

module EvaluationSpec ( spec ) where

import Data.Alphanumeric
import Data.Char
import Data.Data
import Data.GenValidity.Map   ()
import qualified Data.Map     as Map
import Data.MultiSet          ( MultiSet, fromOccurList )
import Data.NonNegative.GenValidity ()
import qualified Data.Set     as Set

import Frugel.Decomposition

import Optics.Extra.Scout

import Prelude                hiding ( one )

import Scout

import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity

i :: Expr
i = unsafeAbstraction "x" $ unsafeVariable "x"

k :: Expr
k = unsafeAbstraction "x" . unsafeAbstraction "y" $ unsafeVariable "x"

s :: Expr
s
    = unsafeAbstraction "f" . unsafeAbstraction "g" . unsafeAbstraction "x"
    $ application' (application' (unsafeVariable "f") (unsafeVariable "x"))
                   (application' (unsafeVariable "g") (unsafeVariable "x"))

runEval' :: (Data a, NodeOf a ~ Node, Decomposable a, Unbound a)
    => (a -> Evaluation a)
    -> a
    -> (a, MultiSet EvaluationError)
runEval' eval = second fst . runEval Nothing Infinity eval

spec :: Spec
spec = describe "Evaluation" $ do
    simpleEvalSpec
    lazinessSpec
    shadowingSpec
    partialApplicationSpec
    expectedFunctionSpec
    expectedIntSpec
    unBoundVariableSpec
    cstrSiteSpec
    abstractionRenamingSpec
    freeVariableCaptureAvoidanceSpec
    freshSuffixSpec

-- Simple test
simpleEvalSpec :: Spec
simpleEvalSpec
    = it "evaluates `s k k q` to `[`q`]` and reports a single UnboundVariableError for `q`"
    $ runEval'
        evalExpr
        (application' (application' (application' s k) k) (unsafeVariable "q"))
    `shouldBe` ( singleExprNodeCstrSite $ unsafeVariable "q"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "q", 1) ]
               )

-- Evaluation is lazy
lazinessSpec :: Spec
lazinessSpec
    = it "lazily evaluates `(\\u = q) ⊥ to `[`q`]` and reports a single UnboundVariableError for `q`"
    $ runEval' evalExpr
               (application' (unsafeAbstraction "u" (unsafeVariable "q"))
                             (error "should not be evaluated"))
    `shouldBe` ( singleExprNodeCstrSite $ unsafeVariable "q"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "q", 1) ]
               )

-- Call-by-need
-- callByNeedTest :: (Expr, MultiSet EvaluationError)
-- callByNeedTest
--     = runEval'
--         (evalExpr
--          . application' (unsafeAbstraction "x"
--                          $ sum' (unsafeVariable "x") (unsafeVariable "x"))
--          . trace "test"
--          $ unsafeVariable "y")
-- also tests laziness
shadowingSpec :: Spec
shadowingSpec
    = it "handles variable shadowing, e.g. `(\\x = \\x = x) q` evaluates to `\\x = x`"
    $ runEval' evalExpr
               (application' (unsafeAbstraction "x"
                              $ unsafeAbstraction "x"
                              $ unsafeVariable "x")
                             (unsafeVariable "q"))
    `shouldBe` (unsafeAbstraction "x" $ unsafeVariable "x", fromOccurList [])

partialApplicationSpec :: Spec
partialApplicationSpec
    = it "continues evaluation in abstraction bodies when they are not fully applied in the result, e.g. `k q` evaluates to `\\y = [`q`]`"
    $ runEval' evalExpr (application' k $ unsafeVariable "q")
    `shouldBe` ( unsafeAbstraction "y" . singleExprNodeCstrSite
                 $ unsafeVariable "q"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "q", 1) ]
               )

expectedFunctionSpec :: Spec
expectedFunctionSpec
    = it "reports a type error when something else than a function is found where a function is expected, e.g. in `(q + q) x`"
    $ runEval' evalExpr
               (application' (sum' (unsafeVariable "q") (unsafeVariable "q"))
                             (unsafeVariable "x"))
    `shouldBe` ( application'
                     (singleExprNodeCstrSite
                      $ sum' (singleExprNodeCstrSite $ unsafeVariable "q")
                             (singleExprNodeCstrSite $ unsafeVariable "q"))
                     (singleExprNodeCstrSite $ unsafeVariable "x")
               , fromOccurList
                     [ ( TypeError . TypeMismatchError Function
                         $ sum' (singleExprNodeCstrSite $ unsafeVariable "q")
                                (singleExprNodeCstrSite $ unsafeVariable "q")
                       , 1
                       )
                     , (UnboundVariableError $ unsafeIdentifier "q", 2)
                     , (UnboundVariableError $ unsafeIdentifier "x", 1)
                     ]
               )

expectedIntSpec :: Spec
expectedIntSpec
    = it "reports a type error when something else than an integer is found where an integer is expected, e.g. in `k + s`"
    $ runEval' evalExpr (sum' k s)
    `shouldBe` ( sum' (singleExprNodeCstrSite k) (singleExprNodeCstrSite s)
               , fromOccurList [ (TypeError $ TypeMismatchError Integer k, 1)
                               , (TypeError $ TypeMismatchError Integer s, 1)
                               ]
               )

unBoundVariableSpec :: Spec
unBoundVariableSpec
    = it "reports an UnboundVariableError when it encounters a unbound variable, e.g. `x`"
    $ runEval' evalExpr (unsafeVariable "x")
    `shouldBe` ( singleExprNodeCstrSite $ unsafeVariable "x"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "x", 1) ]
               )

cstrSiteSpec :: Spec
cstrSiteSpec
    = it "continues evaluation inside construction sites"
    $ runEval'
        evalExpr
        (exprCstrSite'
         $ fromList [ Left 'c'
                    , Right . ExprNode . application' i $ unsafeVariable "x"
                    ])
    `shouldBe` ( exprCstrSite'
                 $ fromList [ Left 'c'
                            , Right . ExprNode . singleExprNodeCstrSite
                              $ unsafeVariable "x"
                            ]
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "x", 1) ]
               )

abstractionRenamingSpec :: Spec
abstractionRenamingSpec
    = it "renames binders to prevent them from capturing variables in expressions substituted into them"
    $ runEval' evalExpr (application' (one "f" "x") (one "f" "x"))
    `shouldBe` (one "x" "x1", mempty)
  where
    one f x
        = unsafeAbstraction f . unsafeAbstraction x
        $ application' (unsafeVariable f) (unsafeVariable x)

freeVariableCaptureAvoidanceSpec :: Spec
freeVariableCaptureAvoidanceSpec
    = it "renames binders to prevent them from capturing free variables"
    $ runEval' evalExpr (application' k $ unsafeVariable "y")
    `shouldBe` ( unsafeAbstraction "y1" . singleExprNodeCstrSite
                 $ unsafeVariable "y"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "y", 1) ]
               )

-- identifiers sizes reduced to increase environment sizes
freshSuffixSpec :: Spec
freshSuffixSpec = modifyMaxSize (* 5) . describe "fresh variable generation" $ do
    it "does not rename a variable when it is not in the environment"
        . forallIdentifiers
        $ \v ->
        forAllShrink (suchThat genValid (not . Map.member v)) shrinkValid
        $ \env -> freshSuffix v (getNonNegative <$> env) `shouldBe` 0
    it "adds a number larger than the number of times the variable was encountered if the variable was already in the environment"
        . forAllValid
        $ \x -> forallIdentifiers $ \v -> forAllShrink
            (Map.insert v x <$> genValid)
            (filter (Map.member v) . shrinkValid)
        $ \env -> freshSuffix v (getNonNegative <$> env)
        `shouldSatisfy` (> getNonNegative x)
    -- maxSuccess increased to catch `freshSuffix (unsafeIdentifier  "x1") $ fromList [(unsafeIdentifier "x", 1)] `shouldSatisfy` (> 0)`
    modifyMaxSuccess (* 100)
        . it "never renames to a variable already in the environment"
        . forallIdentifiers
        $ \v -> forAllValid $ \env -> freshSuffix v (getNonNegative <$> env)
        `shouldSatisfy` (not
                         . flip Set.member (allGeneratedIdentifiers env)
                         . numberedIdentifier v)
  where
    forallIdentifiers
        = forAllSized (`div` 10)
        $ suchThat genValid
                   (not . isDigit . unAlphanumeric . head . view _Identifier)
    allGeneratedIdentifiers
        = Map.foldMapWithKey $ \identifier (NonNegative x) -> fromList
        $ map (numberedIdentifier identifier) [ 0 .. x ]

forAllSized :: (Show a, Testable prop, GenValid a)
    => (Int -> Int)
    -> Gen a
    -> (a -> prop)
    -> Property
forAllSized resizer gen
    = forAllShrink (sized $ flip resize gen . resizer) shrinkValid
