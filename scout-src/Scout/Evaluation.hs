{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Scout.Evaluation where

import Control.Lens.Plated
import Control.Limited
-- Use of lazy writer is important for preserving laziness of evaluation
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

-- The reader being on top of the writer is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
-- LimiterT is outside ScopedEvaluation to preserve sharing in EvaluationEnv (having the values in the env be function would turn the evaluation into call-by-name instead of call-by-need)
type Evaluation = ReaderT EvaluationEnv (LimiterT ScopedEvaluation)

-- Carries Evaluation to be preserve laziness of evaluation
type EvaluationEnv = Map Identifier (Maybe (ScopedEvaluation Expr))

evalCstrSite :: CstrSite -> Evaluation (EvaluationEnv, CstrSite)
evalCstrSite cstrSite = do
    newEnv <- evalScope
        $ toListOf
            (_CstrSite
             % folded
             % _Right
             % (_DeclNode `summing` _WhereNode % _WhereClause % _2 % folded))
            cstrSite
    newCstrSite <- local (const newEnv)
        $ cstrSite & _CstrSite % traversed % _Right %%~ \case
            ExprNode e -> ExprNode <$> evalExpr e
            DeclNode _ -> pure . DeclNode . set (declMeta % #elided) True
                $ declCstrSite' mempty
            WhereNode _ ->
                pure . WhereNode . set (whereClauseMeta % #elided) True
                $ whereCstrSite' mempty
    pure (newEnv, newCstrSite)

-- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
evalExpr :: Expr -> Evaluation Expr
evalExpr v@(Variable _ identifier) = do
    valueBinding <- asks $ Map.lookup identifier
    case valueBinding of
        Nothing -> writer' (singleExprNodeCstrSite v) . MultiSet.singleton
            $ UnboundVariableError identifier
        Just Nothing -> pure v
        Just (Just value) -> lift2 value
evalExpr (Application meta f x) = do
    -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
    (ef, errors) <- mapReaderT (mapLimiterT (pure . runWriter)) $ evalExpr f
    case ef of
        Abstraction
            AbstractionMeta{reified = Just (Hidden reifiedFunction)}
            _
            _ -> draw successfulApplication >>= maybe applicationStub pure
          where
            applicationStub
                = writer' placeholder . MultiSet.singleton . OutOfFuelError
                $ Application meta ef x
            placeholder
                = exprCstrSite'
                $ fromList [ Right . ExprNode $ Application meta ef x ]
            successfulApplication = do
                fuel <- askLimit
                Limited <.> mapReaderT (reifiedFunction . usingLimiterT fuel)
                    $ evalExpr x
        _ -> do
            tell errors
            Application meta <$> reportAnyTypeErrors Function ef <*> evalExpr x -- this makes application strict when there is a type error. Even though this is not "in line" with normal evaluation I do think it's better this way in a practical sense, especially because evaluation is error tolerant
evalExpr (Abstraction meta n e) = do
    reifiedFunction <- asks $ \env x -> usingReaderT (Map.insert n (Just x) env)
        $ evalExpr e
    Abstraction (meta & #reified ?~ Hidden reifiedFunction) n
        <$> local (Map.insert n Nothing) (evalExpr e) -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
-- evalExpr i@(LitN _) = pure i
evalExpr (Sum meta x y) = do
    ex <- evalExpr x
    ey <- evalExpr y
    uncurry (Sum meta)
        <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
    -- case (ex, ey) of
    --     (LitN a, LitN b) -> pure $ LitN (a + b)
    --     _ -> uncurry sum'
    --         <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
evalExpr (ExprCstrSite meta cstrSite)
    = ExprCstrSite meta . snd <$> evalCstrSite cstrSite

evalWhereClause :: WhereClause -> Evaluation (EvaluationEnv, Maybe WhereClause)
evalWhereClause (WhereClause _ decls) = (, Nothing) <$> evalScope decls
evalWhereClause (WhereCstrSite meta cstrSite)
    = second (Just . WhereCstrSite meta) <$> evalCstrSite cstrSite

evalScope :: Foldable t => t Decl -> Evaluation EvaluationEnv
evalScope decls = do
    tell . MultiSet.fromList $ map ConflictingDefinitionsError repeated
    newEnv <- asks computeNewEnv
    fuel <- askLimit
    pure $ usingLimiter fuel newEnv
  where
    repeated
        = mapMaybe (preview $ _tail % _head) . group . sort
        $ toListOf (folded % #name) decls
    computeNewEnv env = newEnv
      where
        newEnv
            = mappend env
            <$> traverse
                evalExpr'
                (fromList (decls ^.. folded % ((,) <$^> #name <*^> #value)))
        evalExpr' e
            = fromMaybe (Just evaluationStub) <$> draw successfulEvaluation
          where
            successfulEvaluation = do
                iteratedEnv <- newEnv
                fuel <- askLimit
                pure
                    . Limited
                    . Just
                    . usingLimiterT fuel
                    . usingReaderT iteratedEnv
                    $ evalExpr e
            evaluationStub
                = writer' (exprCstrSite' $ fromList [ Right $ ExprNode e ])
                . MultiSet.singleton
                $ OutOfFuelError e

evalProgram :: Program -> Evaluation Program
evalProgram program = case program of
    Program{..} -> do
        evaluatedWhereClause <- traverse evalWhereClause whereClause
        newEnv <- asks $ \env -> maybe env fst evaluatedWhereClause
        newExpr <- local (const newEnv) $ evalExpr expr
        pure
            $ program
            & #expr .~ newExpr
            & #whereClause .~ Nothing
            & programMeta % #standardMeta % #interstitialWhitespace .~ [ "" ]
    ProgramCstrSite meta cstrSite ->
        ProgramCstrSite meta . snd <$> evalCstrSite cstrSite

-- fuel is used at applications and recursion and specifies a depth of the computation rather than a length
runEval :: Data a => Limit -> Evaluation a -> (a, MultiSet EvaluationError)
runEval fuel
    = transformOnOf (template @_ @Expr)
                    uniplate
                    (_Abstraction % _1 % #reified .~ Nothing)
    . runWriter
    . usingLimiterT fuel
    . usingReaderT mempty

reportAnyTypeErrors :: MonadWriter (MultiSet EvaluationError) f
    => ExpectedType
    -> Expr
    -> f Expr
reportAnyTypeErrors expectedType e
    = maybe
        (pure e)
        (writer' (singleExprNodeCstrSite e) . MultiSet.singleton . TypeError)
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
