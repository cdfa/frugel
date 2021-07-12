{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Test where

import Data.Hidden
import Data.Map

data LamN
    = VarN Name
    | AppN LamN LamN
    | AbsN (Maybe (Hidden (LamN -> EvaluationMonad LamN))) Name LamN
    | LitN Integer
    | PLusN LamN LamN
    deriving ( Eq, Show )

type Name = String

type EvaluationEnv = Map Name LamN

type EvaluationMonad = Reader EvaluationEnv

-- Just returning v in case it's not in the environment is cool because it kills 2 powerful birds with 1 stone: it adds tolerance for binding errors and it makes it possible to print partially applied functions
eval :: LamN -> EvaluationMonad LamN
eval e@(VarN n) = asks $ fromMaybe e . lookup n
eval (AppN f x) = do
    ef <- eval f
    ex <- eval x
    case ef of
        AbsN (Just (Hidden liftedFunction)) _ _ -> liftedFunction ex
        _ -> return $ AppN ef ex
eval (AbsN _ n e) = ask >>= \env ->
    AbsN (Just . Hidden $ \x -> local (const $ insert n x env) $ eval e) n
    <$> local (delete n) (eval e)  -- delete n from environment, because otherwise a shadowed variable may be used
eval i@(LitN _) = return i
eval (PLusN x y) = do
    ex <- eval x
    ey <- eval y
    return $ case (ex, ey) of
        (LitN a, LitN b) -> LitN (a + b)
        _ -> PLusN ex ey

runEval :: LamN -> LamN
runEval = usingReader mempty . eval

k :: LamN
k = AbsN Nothing "x" $ AbsN Nothing "_" $ VarN "x"

s :: LamN
s
    = AbsN Nothing "f" . AbsN Nothing "g" . AbsN Nothing "x"
    $ AppN (AppN (VarN "f") (VarN "x")) (AppN (VarN "g") (VarN "x"))

-- Simple test
test :: LamN
test = runEval (AppN (AppN (AppN s k) k) (LitN 3))

-- LiftedFunction is lazy
test2 :: LamN
test2 = runEval (AppN (AppN k (LitN 42)) (error "should not be evaluated"))

-- LiftedFunction gets as far as possible with partial application
test3 :: LamN
test3 = runEval (AppN k $ LitN 5)

-- Call-by-need
test4 :: LamN
test4
    = runEval (AppN (AbsN Nothing "x" $ PLusN (VarN "x") (VarN "x"))
               $ trace "test"
               $ LitN 1)

shadowingTest :: LamN
shadowingTest
    = runEval
    $ AppN (AbsN Nothing "x" $ AbsN Nothing "x" $ VarN "x")
           (error "should not be evaluated")
