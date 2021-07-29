{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Test where

import Data.Hidden
import Data.Map

data LamN
    = VarN Name
    | AppN LamN LamN
    | AbsN (Maybe (Hidden (LamN -> LamN))) Name LamN
    | LitN Integer
    | PLusN LamN LamN
    deriving ( Eq, Show )

type Name = String

type EvaluationEnv = Map Name LamN

-- Just returning v in case it's not in the environment is cool because it kills 2 powerful birds with 1 stone: it adds tolerance for binding errors and it makes it possible to print partially applied functions
eval :: EvaluationEnv -> LamN -> LamN
eval env e@(VarN n) = fromMaybe e $ lookup n env
eval env (AppN f x) = let ex = eval env x in case eval env f of
    AbsN (Just (Hidden liftedFunction)) _ _ -> liftedFunction ex
    ef -> AppN ef ex -- this makes application strict when there is a type error. Even though this is not "in line" with normal evaluation I do think it's better this way in a practical sense, especially because evaluation is error tolerant
eval env (AbsN _ n e)
    = AbsN (Just . Hidden $ \x -> eval (insert n x env) e) n
    $ eval (delete n env) e -- delete n from environment, because otherwise a shadowed variable may be used
eval _ i@(LitN _) = i
eval env (PLusN x y) = case (eval env x, eval env y) of
    (LitN a, LitN b) -> LitN (a + b)
    (ex, ey) -> PLusN ex ey

k :: LamN
k = AbsN Nothing "x" $ AbsN Nothing "_" $ VarN "x"

s :: LamN
s
    = AbsN Nothing "f" . AbsN Nothing "g" . AbsN Nothing "x"
    $ AppN (AppN (VarN "f") (VarN "x")) (AppN (VarN "g") (VarN "x"))

-- Simple test
test :: LamN
test = eval mempty (AppN (AppN (AppN s k) k) (LitN 3))

-- LiftedFunction is lazy
test2 :: LamN
test2 = eval mempty (AppN (AppN k (LitN 42)) (error "should not be evaluated"))

-- LiftedFunction gets as far as possible with partial application
test3 :: LamN
test3 = eval mempty (AppN k $ LitN 5)

-- Call-by-need
test4 :: LamN
test4
    = eval mempty
           (AppN (AbsN Nothing "x" $ PLusN (VarN "x") (VarN "x"))
            $ trace "test"
            $ LitN 1)

shadowingTest :: LamN
shadowingTest
    = eval mempty
    $ AppN (AbsN Nothing "x" $ AbsN Nothing "x" $ VarN "x")
           (error "should not be evaluated")
