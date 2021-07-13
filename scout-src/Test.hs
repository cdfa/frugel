{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test where

-- todo move to prelude
import Control.Monad.Writer

import Data.Hidden
import Data.Map       as Map hiding ( filter, fromList )
import Data.MultiSet  as MultiSet hiding ( fromList, join )

import Frugel.CstrSite

import Optics.Extra

data LamN
    = VarN Name
    | AppN LamN LamN
    | AbsN (Maybe (Hidden (ScopedEvaluation LamN -> ScopedEvaluation LamN))) -- argument is scoped to allow shadowing and return value is scoped because the functions scope comes from outside this function
           Name
           LamN
    | LitN Integer
    | PLusN LamN LamN
    | CstrSiteN (ACstrSite LamN)
    deriving ( Eq, Show, Ord )

type Name = String

-- Carries Evaluation to be preserve laziness of evaluation
type EvaluationEnv = Map Name (Maybe (ScopedEvaluation LamN))

-- The reader being on top is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
type Evaluation a = ReaderT EvaluationEnv ScopedEvaluation a

-- For making explicit that something should not be given a environment, but gets it from it's scope
-- Use MultiSets until errors have locations (probably easiest to do with abstract syntax graph with error nodes)
type ScopedEvaluation = Writer (MultiSet EvaluationError)

data EvaluationError = TypeError TypeError | UnboundVariableError Name
    deriving ( Eq, Show, Ord )

data TypeError = TypeMismatchError ExpectedType LamN
    deriving ( Eq, Show, Ord )

data ExpectedType = Function | Integer
    deriving ( Eq, Show, Ord )

makePrisms ''LamN

eval :: LamN -> Evaluation LamN

-- Just returning v in case it's not in the environment is cool because it kills 2 powerful birds with 1 stone: it adds tolerance for binding errors and it makes it possible to print partially applied functions
eval v@(VarN n) = do
    value <- asks $ lookup n
    when (isNothing value) . tell . MultiSet.singleton $ UnboundVariableError n
    lift . fromMaybe (pure v) $ join value
eval (AppN f x) = do
    -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
    (ef, errors) <- mapReaderT (pure . runWriter) $ eval f
    case ef of
        AbsN (Just (Hidden liftedFunction)) _ _ ->
            mapReaderT liftedFunction $ eval x
        _ -> do
            tell errors
            ex <- eval x
            AppN ef <$> reportAnyTypeErrors Function ex
eval (AbsN _ n e) = ask >>= \env ->
    AbsN (Just . Hidden $ \x -> usingReaderT (Map.insert n (Just x) env)
          $ eval e) n <$> local (Map.insert n Nothing) (eval e) -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
eval i@(LitN _) = pure i
eval (PLusN x y) = do
    ex <- eval x
    ey <- eval y
    case (ex, ey) of
        (LitN a, LitN b) -> pure $ LitN (a + b)
        _ -> uncurry PLusN
            <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
eval e@(CstrSiteN _) = e & _CstrSiteN % _CstrSite % traversed % _Right %%~ eval

runEval :: LamN -> (LamN, MultiSet EvaluationError)
runEval = runWriter . usingReaderT mempty . eval

reportAnyTypeErrors :: MonadWriter (MultiSet EvaluationError) f
    => ExpectedType
    -> LamN
    -> f LamN
reportAnyTypeErrors expectedType e
    = maybe
        (pure e)
        ((CstrSiteN (one $ Right e) <$) . tell . MultiSet.singleton . TypeError)
    $ typeCheck e expectedType

typeCheck :: LamN -> ExpectedType -> Maybe TypeError
typeCheck e expectedType = case (e, expectedType) of
    (VarN{}, _) -> Nothing
    (AppN{}, _) -> Nothing
    (CstrSiteN{}, _) -> Nothing
    (AbsN{}, Function) -> Nothing
    (LitN{}, Integer) -> Nothing
    (PLusN{}, Integer) -> Nothing
    _ -> Just $ TypeMismatchError expectedType e

k :: LamN
k = AbsN Nothing "x" $ AbsN Nothing "_" $ VarN "x"

s :: LamN
s
    = AbsN Nothing "f" . AbsN Nothing "g" . AbsN Nothing "x"
    $ AppN (AppN (VarN "f") (VarN "x")) (AppN (VarN "g") (VarN "x"))

-- Simple test
test :: (LamN, MultiSet EvaluationError)
test = runEval (AppN (AppN (AppN s k) k) (LitN 3))

-- Evaluation is lazy
lazinessTest :: (LamN, MultiSet EvaluationError)
lazinessTest
    = runEval (AppN (AbsN Nothing "_" (LitN 42))
                    (error "should not be evaluated"))

-- Evaluation gets as far as possible with partial application
errorToleranceTest :: (LamN, MultiSet EvaluationError)
errorToleranceTest = runEval (AppN k $ LitN 5)

-- Call-by-need
callByNeedTest :: (LamN, MultiSet EvaluationError)
callByNeedTest
    = runEval (AppN (AbsN Nothing "x" $ PLusN (VarN "x") (VarN "x"))
               $ trace "test"
               $ VarN "y")

-- also tests laziness
shadowingTest :: (LamN, MultiSet EvaluationError)
shadowingTest
    = runEval
    $ AppN (AbsN Nothing "x" $ AbsN Nothing "x" $ VarN "x")
           (error "should not be evaluated")

expectedFunctionTest :: (LamN, MultiSet EvaluationError)
expectedFunctionTest = runEval $ AppN (LitN 0) (LitN 0)

expectedIntTest :: (LamN, MultiSet EvaluationError)
expectedIntTest = runEval $ PLusN k s

unBoundVariableTest :: (LamN, MultiSet EvaluationError)
unBoundVariableTest = runEval $ VarN "x"

cstrSiteTest :: (LamN, MultiSet EvaluationError)
cstrSiteTest
    = runEval
    $ CstrSiteN
    $ fromList [ Left 'c', Right $ PLusN (LitN 2) (LitN 3) ]
