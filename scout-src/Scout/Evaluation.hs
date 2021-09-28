{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scout.Evaluation where

import Control.Lens.Plated
import Control.Limited
-- Use of lazy writer is important for preserving laziness of evaluation
import Control.Monad.Writer hiding ( Sum )

import Data.Data
import Data.Data.Lens
import Data.Has
import Data.Hidden
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as Map.Monoidal
import Data.MultiSet    ( MultiSet )
import qualified Data.MultiSet as MultiSet
import Data.Semigroup   ( Max(..) )

import Frugel           hiding ( group )

import Optics.Extra.Scout

import Relude           ( group )
import qualified Relude.Unsafe as Unsafe

import qualified Scout.Internal.Node as Internal
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program

-- The reader being on top of the writer is important, because this allows for working over the result and errors without evaluating something (I think). This is exactly what liftedFunction does
-- LimiterT is outside ScopedEvaluation to preserve sharing in EvaluationEnv (having the values in the env be function would turn the evaluation into call-by-name instead of call-by-need)
type Evaluation = ReaderT EvaluationEnv (LimiterT ScopedEvaluation)

-- Carries Evaluation to be preserve laziness of evaluation
-- Map also stores number of times an argument is shadowed for normalising (\f x -> f x)(\f x -> f x) to \x1 x -> x1 x
type EvaluationEnv = Map Identifier (Either (ScopedEvaluation Expr) Int)

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
            DeclNode _ -> pure $ DeclNode elidedDecl
            WhereNode _ -> pure $ WhereNode elidedWhereClause
    pure (newEnv, newCstrSite)

evalExpr :: Expr -> Evaluation Expr
evalExpr expr
    = do
        ex <- evalExpr' expr
        if expr ^. hasLens @Meta % #focused
            then pure ex
                <&> hasLens @Meta % #focusedNodeValues
                %~ (Hidden (ExprNode (ex
                                      & hasLens @Meta % #focusedNodeValues
                                      .~ mempty)) :<) -- removing the focused node values from the reported expression prevents double reports due to use of template in collecting reports
            else pure ex
  where
    -- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
    evalExpr' v@(Variable meta identifier) = do
        valueBinding <- asks $ Map.lookup identifier
        case valueBinding of
            Nothing -> writerFragment' #errors (singleExprNodeCstrSite v)
                . MultiSet.singleton
                $ UnboundVariableError identifier
            -- It's important that `i` is not yet inspected until after `pure`, because it's dependent on any binders that might be reported and evaluation will diverge (also due to mdo at Abstraction case)
            Just (Right i) ->
                pure . Variable meta $ numberedIdentifier identifier i
            Just (Left value) -> lift2 value
    evalExpr' (Application meta f x) = do
        -- "Catch" errors to preserve laziness in evaluation of unapplied function. If ef is a function, the errors will be raised when liftedFunction evaluates the body, so only force errors in case ef is not a function
        ((efValues, ef), errors) <- mapReaderT (mapLimiterT (pure . runWriter))
            . fmap splitFocusedNodeValues
            $ evalExpr f
        case ef of
            Abstraction
                AbstractionMeta{reified = Just (Hidden reifiedFunction)}
                _
                _ -> draw successfulApplication >>= maybe applicationStub pure
                <&> hasLens @Meta % #focusedNodeValues %~ (efValues <>)
              where
                applicationStub
                    = writerFragment' #errors placeholder
                    . MultiSet.singleton
                    . OutOfFuelError
                    $ Application meta ef x
                placeholder
                    = exprCstrSite'
                    $ fromList [ Right . ExprNode $ Application meta ef x ]
                successfulApplication = do
                    fuel <- askLimit
                    Limited
                        <.> mapReaderT (reifiedFunction . usingLimiterT fuel)
                        $ evalExpr x
            _ -> do
                tell errors
                (exValues, ex) <- splitFocusedNodeValues
                    <$> evalExpr x -- this makes application strict when there is a type error. Even though this is not "in line" with normal evaluation I do think it's better this way in a practical sense, especially because evaluation is error tolerant
                let newMeta
                        = meta
                        & hasLens @Meta % #focusedNodeValues
                        .~ efValues <> exValues
                Application newMeta <$> reportAnyTypeErrors Function ef ?? ex
    evalExpr' (Abstraction meta n e) = mdo
        reifiedFunction <- asks $ \env x ->
            usingReaderT (Map.insert n (Left x) env) $ evalExpr e
        (ex, Max i) <- listening (#binders % at n % to (fromMaybe 0))
            . local (Map.insert n $ Right i) -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
            $ evalExpr e
        tellFragment #binders . Map.Monoidal.singleton n . Max $ succ i
        pure
            $ Abstraction (meta & #reified ?~ Hidden reifiedFunction)
                          (numberedIdentifier n i)
                          ex
    -- evalExpr i@(LitN _) = pure i
    evalExpr' (Sum meta x y) = do
        ex <- evalExpr x
        ey <- evalExpr y
        uncurry (Sum meta)
            <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
        -- case (ex, ey) of
        --     (LitN a, LitN b) -> pure $ LitN (a + b)
        --     _ -> uncurry sum'
        --         <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
    evalExpr' (ExprCstrSite meta cstrSite)
        = ExprCstrSite meta . snd <$> evalCstrSite cstrSite

evalWhereClause :: WhereClause -> Evaluation (EvaluationEnv, Maybe WhereClause)
evalWhereClause (WhereClause _ decls) = (, Nothing) <$> evalScope decls
evalWhereClause (WhereCstrSite meta cstrSite)
    = second (Just . WhereCstrSite meta) <$> evalCstrSite cstrSite

evalScope :: Foldable t => t Decl -> Evaluation EvaluationEnv
evalScope decls = do
    tellFragment #errors . MultiSet.fromList
        $ map ConflictingDefinitionsError repeated
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
            = fromMaybe (Left evaluationStub) <$> draw successfulEvaluation
          where
            successfulEvaluation = do
                iteratedEnv <- newEnv
                fuel <- askLimit
                pure
                    . Limited
                    . Left
                    . usingLimiterT fuel
                    . usingReaderT iteratedEnv
                    $ evalExpr e
            evaluationStub
                = writerFragment'
                    #errors
                    (exprCstrSite' $ fromList [ Right $ ExprNode e ])
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
runEval :: forall a.
    (Data a, Decomposable a, NodeOf a ~ Node)
    => Maybe Int
    -> Limit
    -> (a -> Evaluation a)
    -> a
    -> (a, (MultiSet EvaluationError, Seq Node))
runEval cursorOffset fuel eval
    = removeReifiedFunctions
    . (\(n, output) -> (n, (view #errors output, collectNodeValues n)))
    . runWriter
    . usingLimiterT fuel
    . usingReaderT mempty
    . eval
    . maybe id focusNodeUnderCursor cursorOffset
  where
    removeReifiedFunctions
        = transformOnOf (template @_ @Expr)
                        uniplate
                        (_Abstraction % _1 % #reified .~ Nothing)
    collectNodeValues
        = fmap (view _Hidden)
        . foldOf (foldVL (template @a @Meta) % #focusedNodeValues)

splitFocusedNodeValues :: Has Meta n => n -> (Seq (Hidden Node), n)
splitFocusedNodeValues = hasLens @Meta % #focusedNodeValues <<.~ mempty

reportAnyTypeErrors :: MonadWriter Internal.EvaluationOutput f
    => ExpectedType
    -> Expr
    -> f Expr
reportAnyTypeErrors expectedType e
    = maybe (pure e)
            (writerFragment' #errors (singleExprNodeCstrSite e)
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
