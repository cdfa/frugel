{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Scout.Evaluation where

import Control.Lens.Plated
import Control.Monad.Writer.Strict hiding ( Sum )

import Data.Data
import Data.Data.Lens
import Data.Hidden
import Data.Map              as Map
    hiding ( filter, fromList, map, mapMaybe, toList )
import Data.MultiSet         as MultiSet
    hiding ( fromList, join, map, mapMaybe, toList )

import Optics.Extra

import qualified Relude.Unsafe as Unsafe

import qualified Scout.Internal.Node
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program

-- Carries Evaluation to be preserve laziness of evaluation
type EvaluationEnv = Map Identifier (Maybe (ScopedEvaluation Expr))

-- The reader being on top is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
type Evaluation a = ReaderT EvaluationEnv ScopedEvaluation a

class Evaluatable a where
    eval :: a -> Evaluation a

instance Evaluatable Expr where
    -- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
    eval v@(Variable _ identifier) = do
        valueBinding <- asks $ lookup identifier
        case valueBinding of
            Nothing -> singleExprNodeCstrSite v
                <$ tell (MultiSet.singleton $ UnboundVariableError identifier)
            Just Nothing -> pure v
            Just (Just value) -> lift value
    eval (Application meta f x) = do
        -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
        (ef, errors) <- mapReaderT (pure . runWriter) $ eval f
        case ef of
            Abstraction AbstractionMeta{value = Just (Hidden liftedFunction)}
                        _
                        _ -> mapReaderT liftedFunction $ eval x
            _ -> do
                tell errors
                ex <- eval x
                Application meta <$> reportAnyTypeErrors Function ef ?? ex
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
        & _ExprCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode
        %%~ eval

instance Evaluatable Decl where
    eval decl = case decl of
        Decl{..} -> do
            env <- ask
            let newValue
                    = usingReaderT (Map.insert name (Just newValue) env)
                    $ eval value
            set #value <$> lift newValue ?? decl
        DeclCstrSite{} -> decl
            & _DeclCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode
            %%~ eval

instance Evaluatable WhereClause where
    eval = fst <.> evalWhereClause

evalWhereClause :: WhereClause -> Evaluation (WhereClause, EvaluationEnv)
evalWhereClause whereClause = case whereClause of
    WhereClause _ decls -> do
        env <- ask
        -- todo: check no duplicates
        let (newEnv, newDecls)
                = ( env
                    <> fromList
                        (mapMaybe
                             (\(decl, newDecl) ->
                              (
                              , Just . fmap (Unsafe.fromJust . preview #value)
                                $ newDecl
                              )
                              <$> preview #name decl)
                         $ zip (toList decls) (toList newDecls))
                  , fmap (usingReaderT newEnv . eval) decls
                  )
        evaluatedDecls <- traverse lift newDecls
        pure (whereClause & _WhereClause % _2 .~ evaluatedDecls, newEnv)
        -- set (_WhereClause % _2) <$> traverse lift newDecls ?? whereClause
    WhereCstrSite{} -> (,)
        <$> (whereClause
             & _WhereCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode
             %%~ eval)
        <*> ask

instance Evaluatable Program where
    eval program = case program of
        Program{..} -> do
            env <- ask
            (newWhereClause, newEnv) <- maybe (pure (Nothing, env))
                                              (first Just <.> evalWhereClause)
                                              whereClause
            newExpr <- local (const newEnv) $ eval expr
            pure $ program & #expr .~ newExpr & #whereClause .~ newWhereClause
        ProgramCstrSite{} -> program
            & _ProgramCstrSite
            % _2
            % _CstrSite
            % traversed
            % _Right
            % _ExprNode
            %%~ eval

runEval :: (Evaluatable a, Data a) => a -> (a, MultiSet EvaluationError)
runEval
    = transformOnOf (template @_ @Expr)
                    uniplate
                    (_Abstraction % _1 % #value .~ Nothing)
    . runWriter
    . usingReaderT mempty
    . eval

reportAnyTypeErrors :: MonadWriter (MultiSet EvaluationError) f
    => ExpectedType
    -> Expr
    -> f Expr
reportAnyTypeErrors expectedType e
    = maybe
        (pure e)
        ((singleExprNodeCstrSite e <$) . tell . MultiSet.singleton . TypeError)
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
