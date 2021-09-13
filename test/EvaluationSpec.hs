module EvaluationSpec ( spec ) where

import Data.Data
import Data.MultiSet hiding ( fromList )

import Prelude hiding ( one )

import Scout

import Test.Syd

i :: Expr
i = unsafeAbstraction "x" $ unsafeVariable "x"

k :: Expr
k = unsafeAbstraction "x" . unsafeAbstraction "y" $ unsafeVariable "x"

s :: Expr
s
    = unsafeAbstraction "f" . unsafeAbstraction "g" . unsafeAbstraction "x"
    $ application' (application' (unsafeVariable "f") (unsafeVariable "x"))
                   (application' (unsafeVariable "g") (unsafeVariable "x"))

runEval' :: Data a => Evaluation a -> (a, MultiSet EvaluationError)
runEval' = runEval Infinity

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
    substitutionCaptureAvoidanceSpec

-- Simple test
simpleEvalSpec :: Spec
simpleEvalSpec
    = it "evaluates `s k k q` to `[`q`]` and reports a single UnboundVariableError for `q`"
    $ runEval' (evalExpr
                $ application' (application' (application' s k) k)
                               (unsafeVariable "q"))
    `shouldBe` ( singleExprNodeCstrSite $ unsafeVariable "q"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "q", 1) ]
               )

-- Evaluation is lazy
lazinessSpec :: Spec
lazinessSpec
    = it "lazily evaluates `(\\u = q) ⊥ to `[`q`]` and reports a single UnboundVariableError for `q`"
    $ runEval' (evalExpr
                $ application' (unsafeAbstraction "u" (unsafeVariable "q"))
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
    = it "handles variable shadowing, e.g. `(\\x = \\x = x) ⊥` evaluates to `\\x = x`"
    $ runEval' (evalExpr
                $ application' (unsafeAbstraction "x"
                                $ unsafeAbstraction "x"
                                $ unsafeVariable "x")
                               (error "should not be evaluated"))
    `shouldBe` (unsafeAbstraction "x" $ unsafeVariable "x", fromOccurList [])

partialApplicationSpec :: Spec
partialApplicationSpec
    = it "continues evaluation in abstraction bodies when they are not fully applied in the result, e.g. `k q` evaluates to `\\y = [`q`]`"
    $ runEval' (evalExpr $ application' k $ unsafeVariable "q")
    `shouldBe` ( unsafeAbstraction "y" . singleExprNodeCstrSite
                 $ unsafeVariable "q"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "q", 1) ]
               )

expectedFunctionSpec :: Spec
expectedFunctionSpec
    = it "reports a type error when something else than a function is found where a function is expected, e.g. in `(q + q) x`"
    $ runEval' (evalExpr
                $ application' (sum' (unsafeVariable "q") (unsafeVariable "q"))
                               (unsafeVariable "q"))
    `shouldBe` ( application'
                     (singleExprNodeCstrSite
                      $ sum' (singleExprNodeCstrSite $ unsafeVariable "q")
                             (singleExprNodeCstrSite $ unsafeVariable "q"))
                     (singleExprNodeCstrSite $ unsafeVariable "q")
               , fromOccurList
                     [ ( TypeError . TypeMismatchError Function
                         $ sum' (singleExprNodeCstrSite $ unsafeVariable "q")
                                (singleExprNodeCstrSite $ unsafeVariable "q")
                       , 1
                       )
                     , (UnboundVariableError $ unsafeIdentifier "q", 3)
                     ]
               )

expectedIntSpec :: Spec
expectedIntSpec
    = it "reports a type error when something else than an integer is found where an integer is expected, e.g. in `(q + q) x`"
    $ runEval' (evalExpr $ sum' k s)
    `shouldBe` ( sum' (singleExprNodeCstrSite k) (singleExprNodeCstrSite s)
               , fromOccurList [ (TypeError $ TypeMismatchError Integer k, 1)
                               , (TypeError $ TypeMismatchError Integer s, 1)
                               ]
               )

unBoundVariableSpec :: Spec
unBoundVariableSpec
    = it "reports an UnboundVariableError when it encounters a unbound variable, e.g. `x`"
    $ runEval' (evalExpr $ unsafeVariable "x")
    `shouldBe` ( singleExprNodeCstrSite $ unsafeVariable "x"
               , fromOccurList
                     [ (UnboundVariableError $ unsafeIdentifier "x", 1) ]
               )

cstrSiteSpec :: Spec
cstrSiteSpec
    = it "continues evaluation inside construction sites"
    $ runEval'
        (evalExpr . exprCstrSite'
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

substitutionCaptureAvoidanceSpec :: Spec
substitutionCaptureAvoidanceSpec
    = it "renames variables to prevent them being incorrectly captured when substituted into an abstraction normal form"
    $ runEval' (evalExpr $ application' (one "f") (one "f"))
    `shouldBe` (one "x1", mempty)
  where
    one fName
        = unsafeAbstraction fName . unsafeAbstraction "x"
        $ application' (unsafeVariable fName) (unsafeVariable "x")
