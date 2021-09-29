{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scout.Evaluation where

import Control.Limited

import Data.Data
import Data.Data.Lens
import Data.Has
import Data.Hidden
import qualified Data.Map as Map
import Data.MultiSet    ( MultiSet )
import qualified Data.MultiSet as MultiSet

import Frugel           hiding ( group )

import Optics.Extra.Scout

import Relude           ( group )
import qualified Relude.Unsafe as Unsafe

import qualified Scout.Internal.Node as Internal
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program

-- Evaluation output is saved in Meta and is handled manually.
-- It is possible to use a Writer as well, but this can lead to unexpectedly discarded output (due to preserving laziness) and focused node values would need to be handled manually anyway.
type Evaluation = ReaderT EvaluationEnv Limiter

-- Map also stores number of times an argument is shadowed for normalising (\f x -> f x)(\f x -> f x) to \x1 x -> x1 x
type EvaluationEnv = Map Identifier (Either Expr Int)

evalCstrSite :: CstrSite
    -> Evaluation (MultiSet EvaluationError, EvaluationEnv, CstrSite)
evalCstrSite cstrSite = do
    (errors, newEnv) <- evalScope
        $ toListOf
            (_CstrSite
             % folded
             % _Right
             % (_DeclNode `summing` _WhereNode % _WhereClause % _2 % folded))
            cstrSite
    newCstrSite <- local (const newEnv)
        $ cstrSite & _CstrSite % traversed % _Right %%~ \case
            ExprNode e -> ExprNode <$> evalExpr e
            DeclNode _ -> pure $ DeclNode elidedDecl
            WhereNode _ -> pure $ WhereNode elidedWhereClause
    pure (errors, newEnv, newCstrSite)

evalExpr :: Expr -> Evaluation Expr
evalExpr expr = do
    ex <- evalExpr' expr
    if expr ^. hasLens @Meta % #focused
        then pure ex
            <&> hasLens @Meta % #evaluationOutput % #focusedNodeValues
            %~ (Hidden (ExprNode
                            (ex & hasLens @Meta % #evaluationOutput .~ mempty)) :<) -- removing the focused node values from the reported expression prevents double reports due to use of template in collecting reports
        else pure ex
  where
    -- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
    evalExpr' v@(Variable meta identifier) = do
        valueBinding <- asks $ Map.lookup identifier
        pure $ case valueBinding of
            Nothing -> withErrors
                (MultiSet.singleton $ UnboundVariableError identifier)
                (singleExprNodeCstrSite v)
            Just (Right i) -> Variable meta $ numberedIdentifier identifier i
            Just (Left value) -> value
    evalExpr' (Application meta f x) = do
        (efData, ef) <- splitEvaluationOutput <$> evalExpr f
        case ef of
            Abstraction
                AbstractionMeta{reified = Just (Hidden reifiedFunction)}
                _
                _ -> (hasLens @Meta % #evaluationOutput %~ (efData <>))
                . fromMaybe applicationStub
                <$> draw successfulApplication
              where
                applicationStub
                    = withErrors (MultiSet.singleton . OutOfFuelError
                                  $ Application meta ef x)
                                 placeholder
                placeholder
                    = exprCstrSite'
                    $ fromList [ Right . ExprNode $ Application meta ef x ]
                successfulApplication = do
                    fuel <- askLimit
                    Limited
                        <.> mapReaderT (reifiedFunction . usingLimiter fuel)
                        $ evalExpr x
            _ -> Application newMeta checkedEf <$> evalExpr x -- this makes application strict when there is a type error. Even though this is not "in line" with normal evaluation I do think it's better this way in a practical sense, especially because evaluation is error tolerant
              where
                newMeta
                    = meta
                    & #standardMeta % #evaluationOutput
                    .~ (efData & #errors %~ (errors <>))
                (checkedEf, errors) = reportAnyTypeErrors Function ef
    evalExpr' (Abstraction meta n e) = do
        reifiedFunction <- asks $ \env x ->
            usingReaderT (Map.insert n (Left x) env) $ evalExpr e
        Abstraction (meta & #reified ?~ Hidden reifiedFunction)
                    (numberedIdentifier n 0)
            -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
            <$> local (Map.insert n $ Right 0) (evalExpr e)
    -- evalExpr i@(LitN _) = pure i
    evalExpr' (Sum meta x y) = do
        (ex, exErrors) <- reportAnyTypeErrors Integer <$> evalExpr x
        (ey, eyErrors) <- reportAnyTypeErrors Integer <$> evalExpr y
        pure . withErrors (exErrors <> eyErrors) $ Sum meta ex ey
        -- case (ex, ey) of
        --     (LitN a, LitN b) -> pure $ LitN (a + b)
        --     _ -> uncurry sum'
        --         <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
    evalExpr' (ExprCstrSite meta cstrSite) = do
        (errors, _, eCstrSite) <- evalCstrSite cstrSite
        pure . withErrors errors $ ExprCstrSite meta eCstrSite

evalWhereClause :: WhereClause -> Evaluation (EvaluationEnv, WhereClause)
evalWhereClause (WhereClause _ decls) = do
    (errors, newEnv) <- evalScope decls
    pure ( newEnv
         , elidedWhereClause
           & hasLens @Meta % #evaluationOutput % #errors .~ errors
         )
evalWhereClause (WhereCstrSite meta cstrSite) = do
    (errors, newEnv, eCstrSite) <- evalCstrSite cstrSite
    pure (newEnv, withErrors errors $ WhereCstrSite meta eCstrSite)

evalScope :: Foldable t
    => t Decl
    -> Evaluation (MultiSet EvaluationError, EvaluationEnv)
evalScope decls = do
    newEnv <- asks computeNewEnv
    lift ((MultiSet.fromList $ map ConflictingDefinitionsError repeated, )
          <$> newEnv)
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
            = fromMaybe (Left evaluationStub) <$> draw successfulEvaluation
          where
            successfulEvaluation = do
                iteratedEnv <- newEnv
                Limited <.> Left <.> usingReaderT iteratedEnv $ evalExpr e
            evaluationStub
                = withErrors (MultiSet.singleton (OutOfFuelError e))
                . exprCstrSite'
                $ fromList [ Right $ ExprNode e ]

evalProgram :: Program -> Evaluation Program
evalProgram program@Program{..} = do
    evaluatedWhereClause <- traverse evalWhereClause whereClause
    (newEnv, eWhereClauseData) <- asks $ \env -> maybe
        (env, mempty)
        (second . view $ hasLens @Meta % #evaluationOutput)
        evaluatedWhereClause
    newExpr <- local (const newEnv) $ evalExpr expr
    pure
        $ program
        & #expr .~ newExpr
        & #whereClause .~ Nothing
        & programMeta % #standardMeta
        %~ (#interstitialWhitespace .~ [ "" ])
        . (#evaluationOutput .~ eWhereClauseData)
evalProgram (ProgramCstrSite meta cstrSite) = do
    (errors, _, eCstrSite) <- evalCstrSite cstrSite
    pure . withErrors errors $ ProgramCstrSite meta eCstrSite

-- fuel is used at applications and recursion and specifies a depth of the computation rather than a length
runEval :: (Data a, Decomposable a, NodeOf a ~ Node)
    => Maybe Int
    -> Limit
    -> (a -> Evaluation a)
    -> a
    -> (a, (MultiSet EvaluationError, Seq Node))
runEval cursorOffset fuel eval
    = removeReifiedFunctions
    . (\n -> ( removeEvaluationOutput n
             , (MultiSet.fromOccurMap
                . removeEvaluationOutput
                . MultiSet.toMap -- unwrap and rewrap MultiSet newtype because of Data MultiSet instance hiding it's contents to optics
                . view #errors)
               &&& (fmap (view _Hidden) . view #focusedNodeValues)
               $ collectNodeValues n
             ))
    . usingLimiter fuel
    . usingReaderT mempty
    . eval
    . maybe id focusNodeUnderCursor cursorOffset
  where
    removeReifiedFunctions
        = traversalVL (template @_ @AbstractionMeta) % #reified .~ Nothing
    removeEvaluationOutput :: Data a => a -> a
    removeEvaluationOutput
        = traversalVL (template @_ @Meta) % #evaluationOutput .~ mempty
    collectNodeValues = foldOf (foldVL (template @_ @Meta) % #evaluationOutput)

splitEvaluationOutput :: Has Meta n => n -> (EvaluationOutput, n)
splitEvaluationOutput = hasLens @Meta % #evaluationOutput <<.~ mempty

withErrors :: Has Meta n => MultiSet EvaluationError -> n -> n
withErrors errors = hasLens @Meta % #evaluationOutput % #errors %~ (errors <>)

reportAnyTypeErrors :: ExpectedType -> Expr -> (Expr, MultiSet EvaluationError)
reportAnyTypeErrors expectedType expr
    = maybe (expr, mempty) (\err -> ( singleExprNodeCstrSite expr
                                    , MultiSet.singleton $ TypeError err
                                    )) $ typeCheck expr expectedType

typeCheck :: Expr -> ExpectedType -> Maybe TypeError
typeCheck e expectedType = case (e, expectedType) of
    (Variable{}, _) -> Nothing
    (Application{}, _) -> Nothing
    (ExprCstrSite{}, _) -> Nothing
    (Abstraction{}, Function) -> Nothing
    -- (LitN{}, Integer) -> Nothing
    (Sum{}, Integer) -> Nothing
    _ -> Just $ TypeMismatchError expectedType e

numberedIdentifier :: Identifier -> Int -> Identifier
numberedIdentifier identifier 0 = identifier
numberedIdentifier identifier i
    = identifier <> Unsafe.fromJust (identifier' $ show i) -- safe because i is integer

focusNodeUnderCursor :: (Decomposable n, NodeOf n ~ Node) => Int -> n -> n
focusNodeUnderCursor cursorOffset n
    = fromRight n $ modifyNodeAt' (const $ pure . focusNode) cursorOffset n
  where
    focusNode :: (IsNode n, NodeOf n ~ Node) => n -> n
    focusNode = Unsafe.fromJust . preview nodePrism . focus . review nodePrism -- safe because of optics laws and that hasLens @Meta @(NodeOf n) can not change what type of node it is
    focus :: Has Meta n => n -> n
    focus = hasLens @Meta % #focused .~ True
