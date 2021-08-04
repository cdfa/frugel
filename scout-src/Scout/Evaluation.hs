{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Scout.Evaluation where

import Control.Lens.Plated
import Control.Monad.Writer hiding ( Sum )

import Data.Data
import Data.Data.Lens
import Data.Hidden
import qualified Data.Map as Map
import Data.MultiSet    ( MultiSet )
import qualified Data.MultiSet as MultiSet

import Frugel           hiding ( group )

import Optics.Extra

import Relude           ( group )

import qualified Scout.Internal.Node
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program

-- The reader being on top is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
type Evaluation a = ReaderT EvaluationEnv ScopedEvaluation a

-- Carries Evaluation to be preserve laziness of evaluation
type EvaluationEnv = Map Identifier (Maybe (ScopedEvaluation Expr))

class Evaluatable a where
    eval :: a -> Evaluation a

instance Evaluatable Expr where
    -- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
    eval v@(Variable _ identifier) = do
        valueBinding <- asks $ Map.lookup identifier
        case valueBinding of
            Nothing -> singleExprNodeCstrSite v
                <$ tell (MultiSet.singleton $ UnboundVariableError identifier)
            Just Nothing -> pure v
            Just (Just value) -> lift value
    eval (Application meta f x) = do
        -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
        (ef, errors) <- mapReaderT (pure . runWriter) $ eval f
        case ef of
            Abstraction
                AbstractionMeta{reified = Just (Hidden reifiedFunction)}
                _
                _ -> mapReaderT reifiedFunction $ eval x
            _ -> do
                tell errors
                Application meta <$> reportAnyTypeErrors Function ef <*> eval x -- this makes application strict when there is a type error. Even though this is not "in line" with normal evaluation I do think it's better this way in a practical sense, especially because evaluation is error tolerant
    eval (Abstraction meta n e) = do
        reifiedFunction <- asks
            (\env x -> usingReaderT (Map.insert n (Just x) env) $ eval e)
        Abstraction (meta & #reified ?~ Hidden reifiedFunction) n
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
    eval e@(ExprCstrSite _ _) = pure e

        -- & _ExprCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode
        -- %%~ eval
evalDecl :: Decl -> Evaluation (Either EvaluationEnv Decl)
evalDecl Decl{..}
    = asks (\env ->
            let newEnv
                    = Map.insert name
                                 (Just . usingReaderT newEnv $ eval value)
                                 env
            in Left newEnv)
evalDecl decl@DeclCstrSite{} = pure $ Right decl

    -- & _DeclCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode %%~ eval
    -- <&> Right
evalWhereClause :: WhereClause -> Evaluation (Either EvaluationEnv WhereClause)
evalWhereClause (WhereClause _ decls) = do
    tell . MultiSet.fromList $ map ConflictingDefinitionsError repeated
    env <- ask
    let newEnv
            = env
            <> fromList (decls ^.. folded
                         % ((,) <$^> #name <*^> #value
                            % to (Just . usingReaderT newEnv . eval)))
    pure $ Left newEnv
  where
    repeated
        = mapMaybe (preview $ _tail % _head) . group . sort
        $ toListOf (folded % #name) decls
evalWhereClause whereClause@WhereCstrSite{} = pure $ Right whereClause

    -- & _WhereCstrSite % _2 % _CstrSite % traversed % _Right % _ExprNode %%~ eval
    -- <&> Right
instance Evaluatable Program where
    eval program = case program of
        Program{..} -> do
            env <- ask
            evaluatedWhereClause <- traverse evalWhereClause whereClause
            let newEnv = fromMaybe env (evaluatedWhereClause >>= leftToMaybe)
            newExpr <- local (const newEnv) $ eval expr
            pure
                $ program
                & #expr .~ newExpr
                & #whereClause .~ Nothing
                & programMeta % #standardMeta % #interstitialWhitespace
                .~ [ "" ]
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
                    (_Abstraction % _1 % #reified .~ Nothing)
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
