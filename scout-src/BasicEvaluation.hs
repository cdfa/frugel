{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

-- This module is not used anywhere, but it's a nice playground for testing evaluation-related things
module BasicEvaluation where

import Data.Hidden
import Data.Map
import Data.Type.Equality

type Expr = GExpr 'Syntax

data ExprRepresentation = Syntax | Lifted | PartiallyReified

-- todo: make instance explicit using kind-generics so we can remove Hidden
data GExpr a where
    Var :: Name -> GExpr a
    App :: (a /~ 'Lifted) => GExpr a -> GExpr a -> GExpr a
    Abs :: (a /~ 'Lifted) => Name -> GExpr a -> GExpr a
    Lit :: Integer -> GExpr a
    Plus :: GExpr a -> GExpr a -> GExpr a
    LiftedApp :: GExpr 'Lifted
        -> Hidden (Evaluation (GExpr 'Lifted))
        -> GExpr 'Lifted
    LiftedAbs :: Name
        -> Hidden (Evaluation (GExpr 'Lifted) -> Evaluation (GExpr 'Lifted))
        -> GExpr 'Lifted
    GLiftedExpr :: GExpr 'Lifted -> GExpr 'PartiallyReified

infix 4 /~

class (a == b) ~ 'False => (/~) (a :: k) (b :: k)

instance (a == b) ~ 'False => (/~) a b

-- want to make this only work on Syntax, but need kind-generics for it
deriving instance Eq (GExpr a)

deriving instance Show (GExpr a)

-- deriving instance Data (GExpr Syntax)
type Evaluation = IO

type Name = String

type EvaluationEnv = Map Name (Evaluation (GExpr 'Lifted))

interpret :: EvaluationEnv -> Expr -> Evaluation (GExpr 'Lifted)
interpret env (Var n) = fromMaybe (pure $ Var n) $ lookup n env
interpret env (App f x) = let ex = interpret env x in interpret env f >>= \case
    LiftedAbs _ (Hidden liftedFunction) -> liftedFunction ex
    ef -> pure $ LiftedApp ef $ Hidden ex
interpret env (Abs n e)
    = pure $ LiftedAbs n $ Hidden (\x -> interpret (insert n x env) e)
interpret _ (Lit i) = pure $ Lit i
interpret env (Plus x y) = do
    ex <- interpret env x
    ey <- interpret env y
    pure $ case (ex, ey) of
        (Lit a, Lit b) -> Lit (a + b)
        _ -> Plus ex ey

reifyToDepth :: Int -> GExpr 'Lifted -> IO (GExpr 'PartiallyReified)
reifyToDepth 0 = pure . GLiftedExpr
reifyToDepth depth = \case
    Var n -> pure $ Var n
    LiftedApp f (Hidden eArg) -> do
        arg <- eArg
        App <$> reifyToDepth (pred depth) f <*> reifyToDepth depth arg
    LiftedAbs n (Hidden f) -> do
        body <- f $ pure $ Var $ "fresh" ++ n
        Abs n <$> reifyToDepth depth body
    Lit i -> pure $ Lit i
    Plus left right ->
        Plus <$> reifyToDepth depth left <*> reifyToDepth depth right

resumeReificationToDepth
    :: Int -> GExpr 'PartiallyReified -> IO (GExpr 'PartiallyReified)
resumeReificationToDepth 0 = pure
resumeReificationToDepth depth = \case
    GLiftedExpr liftedExpr -> reifyToDepth depth liftedExpr
    _ -> error "not implement yet"

    -- reifiedExpr -> uniplate (resumeReificationToDepth (pred depth)) reifiedExpr
k :: Expr
k = Abs "x" $ Abs "_" $ Var "x"

s :: Expr
s
    = Abs "f" . Abs "g" . Abs "x"
    $ App (App (Var "f") (Var "x")) (App (Var "g") (Var "x"))

-- Simple test
test :: IO (GExpr 'PartiallyReified)
test = reifyToDepth 100 =<< interpret mempty (App (App (App s k) k) (Lit 3))

-- LiftedFunction is lazy
test2 :: IO (GExpr 'PartiallyReified)
test2
    = reifyToDepth 100
    =<< interpret mempty
                  (App (App k (Lit 42)) (error "should not be evaluated"))

-- LiftedFunction gets as far as possible with partial application
test3 :: IO (GExpr 'PartiallyReified)
test3 = reifyToDepth 100 =<< interpret mempty (App k $ Lit 5)

-- Call-by-need
test4 :: IO (GExpr 'PartiallyReified)
test4
    = reifyToDepth 100
    =<< interpret
        mempty
        (App (Abs "x" $ Plus (Var "x") (Var "x")) $ trace "test" $ Lit 1)

shadowingTest :: IO (GExpr 'PartiallyReified)
shadowingTest
    = reifyToDepth 100
    =<< interpret
        mempty
        (App (Abs "x" $ Abs "x" $ Var "x") (error "should not be evaluated"))
