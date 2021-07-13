{-# LANGUAGE FlexibleContexts #-}

module Scout.Evaluation where

-- todo move to prelude
import Control.Monad.Writer hiding ( Sum )

import Data.Hidden
import Data.Map       as Map hiding ( filter, fromList )
import Data.MultiSet  as MultiSet hiding ( fromList, join )

import Optics.Extra

import qualified Scout.Internal.Node
import Scout.Node

-- Carries Evaluation to be preserve laziness of evaluation
type EvaluationEnv = Map Identifier (Maybe (ScopedEvaluation Expr))

-- The reader being on top is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
type Evaluation a = ReaderT EvaluationEnv ScopedEvaluation a

eval :: Expr -> Evaluation Expr

-- Just returning v in case it's not in the environment is cool because it kills 2 powerful birds with 1 stone: it adds tolerance for binding errors and it makes it possible to print partially applied functions
eval v@(Variable _ identifier) = do
    value <- asks $ lookup identifier
    when (isNothing value) . tell . MultiSet.singleton
        $ UnboundVariableError identifier
    lift . fromMaybe (pure v) $ join value
eval (Application meta f x) = do
    -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
    (ef, errors) <- mapReaderT (pure . runWriter) $ eval f
    case ef of
        Abstraction AbstractionMeta{value = Just (Hidden liftedFunction)} _ _ ->
            mapReaderT liftedFunction $ eval x
        _ -> do
            tell errors
            ex <- eval x
            Application meta ef <$> reportAnyTypeErrors Function ex
eval (Abstraction meta n e) = ask >>= \env -> Abstraction
    (meta & #value ?~ Hidden (\x -> usingReaderT (Map.insert n (Just x) env)
                              $ eval e))
    n
    <$> local (Map.insert n Nothing) (eval e) -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
-- eval i@(LitN _) = pure i
eval (Sum meta x y) = do
    ex <- eval x
    ey <- eval y
    uncurry (Sum meta)
        <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
    -- case (ex, ey) of
    --     (LitN a, LitN b) -> pure $ LitN (a + b)
    --     _ -> uncurry sum'
    --         <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
-- todo: extend to all nodes
eval e@(ExprCstrSite _ _)
    = e
    & _ExprCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode %%~ eval

runEval :: Expr -> (Expr, MultiSet EvaluationError)
runEval = runWriter . usingReaderT mempty . eval

reportAnyTypeErrors :: MonadWriter (MultiSet EvaluationError) f
    => ExpectedType
    -> Expr
    -> f Expr
reportAnyTypeErrors expectedType e
    = maybe (pure e)
            ((exprCstrSite' (one . Right $ ExprNode e) <$)
             . tell
             . MultiSet.singleton
             . TypeError)
    $ typeCheck e expectedType

typeCheck :: Expr -> ExpectedType -> Maybe TypeError
typeCheck e expectedType = case (e, expectedType) of
    (Variable{}, _) -> Nothing
    (Application{}, _) -> Nothing
    (ExprCstrSite{}, _) -> Nothing
    (Abstraction{}, Function) -> Nothing
    -- (LitN{}, Integer) -> Nothing
    (Sum{}, Integer) -> Nothing
    _ -> Just $ TypeMismatchError expectedType e

k :: Expr
k = unsafeAbstraction "x" $ unsafeAbstraction "y" $ unsafeVariable "x"

s :: Expr
s
    = unsafeAbstraction "f" . unsafeAbstraction "g" . unsafeAbstraction "x"
    $ application' (application' (unsafeVariable "f") (unsafeVariable "x"))
                   (application' (unsafeVariable "g") (unsafeVariable "x"))

-- Simple test
test :: (Expr, MultiSet EvaluationError)
test
    = runEval (application' (application' (application' s k) k)
                            (unsafeVariable "q"))

-- Evaluation is lazy
lazinessTest :: (Expr, MultiSet EvaluationError)
lazinessTest
    = runEval (application' (unsafeAbstraction "_" (unsafeVariable "q"))
                            (error "should not be evaluated"))

-- Evaluation gets as far as possible with partial application
errorToleranceTest :: (Expr, MultiSet EvaluationError)
errorToleranceTest = runEval (application' k $ unsafeVariable "q")

-- Call-by-need
callByNeedTest :: (Expr, MultiSet EvaluationError)
callByNeedTest
    = runEval (application' (unsafeAbstraction "x"
                             $ sum' (unsafeVariable "x") (unsafeVariable "x"))
               $ trace "test"
               $ unsafeVariable "y")

-- also tests laziness
shadowingTest :: (Expr, MultiSet EvaluationError)
shadowingTest
    = runEval
    $ application'
        (unsafeAbstraction "x" $ unsafeAbstraction "x" $ unsafeVariable "x")
        (error "should not be evaluated")

expectedFunctionTest :: (Expr, MultiSet EvaluationError)
expectedFunctionTest
    = runEval
    $ application' (sum' (unsafeVariable "q") (unsafeVariable "q"))
                   (unsafeVariable "q")

expectedIntTest :: (Expr, MultiSet EvaluationError)
expectedIntTest = runEval $ sum' k s

unBoundVariableTest :: (Expr, MultiSet EvaluationError)
unBoundVariableTest = runEval $ unsafeVariable "x"

cstrSiteTest :: (Expr, MultiSet EvaluationError)
cstrSiteTest
    = runEval
    $ exprCstrSite'
    $ fromList
        [ Left 'c', Right $ ExprNode $ application' k $ unsafeVariable "x" ]
