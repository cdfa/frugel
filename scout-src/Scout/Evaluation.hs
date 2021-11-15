{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scout.Evaluation where

#if defined(ghcjs_HOST_OS)
import Control.Concurrent
#endif
import Control.Limited
import Control.Monad.Writer   hiding ( Sum )

import Data.Alphanumeric
import Data.Char              ( isDigit )
import Data.Data
import Data.Data.Lens
import Data.Has
import Data.Hidden
import qualified Data.Map     as Map
import Data.Map.Optics
import Data.MultiSet          ( MultiSet )
import qualified Data.MultiSet as MultiSet
import qualified Data.Set     as Set
import Data.Set.Optics

import Frugel                 hiding ( Elided, group )

import Optics.Extra.Scout
import Optics.Writer

import Relude                 ( group )
import qualified Relude.Unsafe as Unsafe

import Scout.Internal.EvaluationEnv
    ( EvaluationEnv(EvaluationEnv), _EvaluationEnv )
import qualified Scout.Internal.EvaluationEnv as EvaluationEnv
import qualified Scout.Internal.Node as Node
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Orphans.MultiSet ()
import Scout.Program
import Scout.Unbound

import System.IO.Unsafe

type Evaluation = LimiterT (ReaderT EvaluationEnv ScopedEvaluation)

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
            ExprNode e -> ExprNode . deferEvaluation
                <$> newEvalRef (evalExpr e)
            n -> pure $ elide n
    pure (newEnv, newCstrSite)

evalExpr :: Expr -> Evaluation Expr
evalExpr expr = do
    -- for some reason this ensures threads can be interrupted when compiled to JS where yield and -fno-omit-yields don't
#if defined(ghcjs_HOST_OS)
    liftIO $ threadDelay 1
#endif
    eExpr <- evalExpr' expr
    when (expr ^. hasLens @Meta % #focused)
        . tellFragment #focusedNodeEvaluations
        . one
        =<< focusedNodeEvaluation eExpr
    pure eExpr
  where
    focusedNodeEvaluation eExpr = do
        valuesInScope <- liftIO . traverse (unsafeInterleaveIO . dereference)
            =<< gview #valueEnv
        definitionsInScope
            <- Map.restrictKeys valuesInScope <$> gview #definitions
        variablesInScope
            <- Map.withoutKeys valuesInScope <$> gview #definitions
        pure
            $ review _FocusedNodeEvaluation
                     (definitionsInScope, variablesInScope, ExprNode eExpr)
      where
        dereference evalRef = do
            refContents <- readIORef evalRef
            case refContents of
                Left scopedValue -> do
                    (value, output) <- runWriterT scopedValue
                    writeIORef evalRef . Left $ writer (value, output)
                    pure value
                Right value -> pure value
    evalExpr' v@(Variable _ identifier) = do
        valueRefMaybe <- gview (#valueEnv % at identifier)
        maybe variableStub (lift2 . outputOnce) valueRefMaybe
      where
        variableStub
            = writerFragment #errors
                             ( singleExprNodeCstrSite v
                             , one $ UnboundVariableError identifier
                             )
    evalExpr' (Application meta f x) = do
        ef <- evalExpr f
        case ef of
            Abstraction
                AbstractionMeta{reified = Just (Hidden reifiedFunction)}
                _
                _ -> maybe applicationStub pure =<< draw successfulApplication
              where
                successfulApplication = do
                    xRef <- newEvalRef $ evalExpr x
                    Limited <$> magnify #shadowingEnv (reifiedFunction xRef)
                applicationStub
                    = writerFragment
                        #errors
                        ( exprCstrSite' $ fromList [ Right $ ExprNode newApp ]
                        , one $ OutOfFuelError newApp
                        )
                newApp
                    = Application meta ef
                    $ hasLens @ExprMeta % #evaluationStatus .~ OutOfFuel
                    $ x
            Abstraction{} -> error
                "Internal evaluation error: object language function was missing meta language function"
            _ -> do
                checkedEf <- reportAnyTypeErrors Function ef
                Application meta checkedEf . deferEvaluation
                    <$> newEvalRef (evalExpr x)
    evalExpr' (Abstraction meta n body) = do
        reifiedFunction
            <- asks $ \EvaluationEnv{valueEnv, definitions} arg -> do
                scopedBody <- scope
                    . magnify
                        (to $ \shadowingEnv ->
                         EvaluationEnv { valueEnv = Map.insert n arg valueEnv
                                       , shadowingEnv
                                       , definitions = Set.delete n definitions
                                       })
                    $ evalExpr body
                -- each renameShadowedVariables invocation renames all binders, so only the last one's result's is actually used
                renameShadowedVariables scopedBody
        fmap (Abstraction (meta & #reified ?~ Hidden reifiedFunction) n
              . deferEvaluation)
            . newEvalRef
            $ do
                stubRef <- newIORef . pure $ variable' n
                -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
                local (chain [ #valueEnv % at n ?~ stubRef
                             , #shadowingEnv % at n ?~ 0
                             ])
                    $ evalExpr body
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

normaliseExpr :: Expr -> ScopedEvaluation Expr
normaliseExpr expr
    = mapWriterT unsafeInterleaveIO
    $ case expr ^. hasLens @ExprMeta % #evaluationStatus of
        EvaluationDeferred (Hidden (evalRef, modifiers)) ->
            uniplate normaliseExpr =<< modifiers (outputOnce evalRef)
        Elided _ -> expr
            & hasLens @ExprMeta % #evaluationStatus % _Elided % _Hidden
            %%~ normaliseExpr
        Evaluated -> uniplate normaliseExpr expr
        OutOfFuel -> pure expr

newEvalRef :: MonadIO m
    => LimiterT (ReaderT r ScopedEvaluation) a
    -> LimiterT (ReaderT r m) (EvaluationRef a)
newEvalRef = mapLimiterT . mapReaderT $ newIORef . Left

scope :: Applicative m
    => LimiterT (ReaderT r m) a
    -> LimiterT (ReaderT r m) (m a)
scope = mapLimiterT $ mapReaderT pure

outputOnce :: (MonadIO (t m), MonadTrans t, Monad m, MonadWriter w (t m))
    => IORef (Either (WriterT w m a) a)
    -> t m a
outputOnce valueRef = do
    scopedValue <- readIORef valueRef
    case scopedValue of
        Left withOutput -> do
            (value, output) <- lift $ runWriterT withOutput
            writeIORef valueRef $ Right value
            writer (value, output)
        Right withoutOutput -> pure withoutOutput

evalWhereClause :: WhereClause -> Evaluation (EvaluationEnv, WhereClause)
evalWhereClause whereClause@(WhereClause _ decls)
    = (, whereClause) <$> evalScope decls
evalWhereClause (WhereCstrSite meta cstrSite)
    = second (WhereCstrSite meta) <$> evalCstrSite cstrSite

-- todo: return evaluated decls
-- With this way of implementing limited recursion, the bound expressions get evaluated again with every recursive application, which would not be necessary if recursion was not limited.
-- I can't find a better way to do it for now, because limited recursion implies that the results of evaluation can change (when the limit is reached).
-- You would somehow need to change the value the evaluation of a bound variable resulted in after the fact.
-- Other options include limiting all uses of bound expressions (instead of recursive uses) or maybe something with graph-reduction.
evalScope :: Foldable t => t Decl -> Evaluation EvaluationEnv
evalScope decls = do
    fuelLimit <- askLimit
    evaluatedDecls <- newIORef mempty
    newEnv <- liftIO . usingLimiterT fuelLimit . computeNewEnv evaluatedDecls
        =<< ask
    writerFragment
        #errors
        (newEnv, MultiSet.fromList $ map ConflictingDefinitionsError repeated)
  where
    repeated
        = mapMaybe (preview $ _tail % _head) . group . sort
        $ toListOf (folded % #name) decls
    computeNewEnv evaluatedDecls EvaluationEnv{..}
        = fromValueEnv <$> newValueEnv
      where
        newShadowingEnv
            = shadowingEnv
            <> toMapOf (folded % (#name `fanout` like 0) % ito id) decls
        newDefinitionsSet = definitions <> setOf (folded % #name) decls
        fromValueEnv
            = review _EvaluationEnv . (, newShadowingEnv, newDefinitionsSet)
        newValueEnv
            = mappend valueEnv <.> itraverse evalDecl
            $ toMapOf (folded % (#name `fanout` #value) % ito id) decls
        evalDecl name value = do
            valueEither <- Left . fromMaybe evaluationStub
                <$> draw successfulEvaluation
            newIORef valueEither
          where
            successfulEvaluation = do
                iteratedEnv <- mapLimiterT unsafeInterleaveIO newValueEnv
                Limited
                    <.> mapLimiterT
                        (pure . usingReaderT (fromValueEnv iteratedEnv))
                    $ do
                        evaluated
                            <- Set.member name <$> readIORef evaluatedDecls
                        if evaluated
                            then censoring #errors
                                           (MultiSet.filter
                                                (== recursionLimitReachedError))
                                $ evalExpr value
                            else do
                                modifyIORef evaluatedDecls (Set.insert name)
                                evalExpr value
            evaluationStub :: MonadWriter EvaluationOutput m => m Expr
            evaluationStub
                = writerFragment
                    #errors
                    ( exprCstrSite'
                      $ fromList [ Right . ExprNode $ variable' name ]
                    , one recursionLimitReachedError
                    )
            recursionLimitReachedError = OutOfFuelError $ variable' name

evalProgram :: Program -> Evaluation Program
evalProgram Program{..} = do
    eWhereClause <- traverse evalWhereClause whereClause
    newEnv <- asks $ \env -> maybe env fst eWhereClause
    newExpr <- local (const newEnv) $ evalExpr expr
    pure Program { meta = meta
                       & #standardMeta % #interstitialWhitespace .~ [ "" ]
                 , expr = newExpr
                 , whereClause = Nothing
                 }
evalProgram (ProgramCstrSite meta cstrSite)
    = ProgramCstrSite meta . snd <$> evalCstrSite cstrSite

-- fuel is used at applications and recursion and specifies a depth of the computation rather than a length
-- It's possible to run this in ST instead of IO, but we would need an extra type parameter on ScopedEvaluation and therefor on Expression
-- May be feasible when we use hypertypes to parametrise the tree
runEval :: (Decomposable a, NodeOf a ~ Node, Unbound a, Data a)
    => Maybe Int
    -> Limit
    -> (a -> Evaluation a)
    -> a
    -> IO (a, (MultiSet EvaluationError, Seq FocusedNodeEvaluation))
runEval cursorOffset fuel eval x
    = traverseOf _2
                 ((view #errors &&& view #focusedNodeEvaluations)
                  <.> fst
                  <.> runWriterT . template @_ @Expr normaliseExpr)
    . over (traversalVL (template @_ @Expr)) removeReifiedFunctions
    =<< runWriterT
        (template @_ @Expr normaliseExpr
         =<< (usingReaderT
                  (mempty { EvaluationEnv.shadowingEnv = Map.fromSet (const 0)
                                $ freeVariables mempty x
                          })
              . usingLimiterT fuel
              . eval
              $ maybe id focusNodeUnderCursor cursorOffset x))

removeReifiedFunctions :: Expr -> Expr
removeReifiedFunctions
    expr = case expr ^. hasLens @ExprMeta % #evaluationStatus of
    EvaluationDeferred _ -> expr
        & hasLens @ExprMeta
        % #evaluationStatus
        % _EvaluationDeferred
        % _Hidden
        % _2
        %~ (pure . removeReifiedFunction . removeReifiedFunctions <=<)
    Elided _ -> expr
        & hasLens @ExprMeta % #evaluationStatus % _Elided % _Hidden
        %~ removeReifiedFunction . removeReifiedFunctions
    Evaluated -> removeReifiedFunction
        $ over (traversalVL uniplate) removeReifiedFunctions expr
    OutOfFuel -> expr
  where
    removeReifiedFunction = _Abstraction % _1 % #reified .~ Nothing

reportAnyTypeErrors
    :: MonadWriter EvaluationOutput m => ExpectedType -> Expr -> m Expr
reportAnyTypeErrors expectedType e
    = maybe
        (pure e)
        (writerFragment' #errors (singleExprNodeCstrSite e) . one . TypeError)
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

 -- we don't need to traverse the root node, because it's value is the result of evaluation (and we wouldn't be able to reuse traverseChildNodeAt machinery, because Program is not a Node)
focusNodeUnderCursor :: (Decomposable n, NodeOf n ~ Node) => Int -> n -> n
focusNodeUnderCursor cursorOffset n
    = fromRight n
    $ traverseChildNodeAt (const $ pure . focusNode) cursorOffset n
  where
    focusNode :: (IsNode n, NodeOf n ~ Node) => n -> n
    focusNode = Unsafe.fromJust . preview nodePrism . focus . review nodePrism -- safe because of optics laws and that hasLens @Meta @(NodeOf n) can not change what type of node it is
    focus :: Has Meta n => n -> n
    focus = hasLens @Meta % #focused .~ True

renameShadowedVariables :: ScopedEvaluation Expr
    -> LimiterT (ReaderT ShadowingEnv ScopedEvaluation) Expr
renameShadowedVariables scopedExpr = do
    (expr, output) <- liftIO $ runWriterT scopedExpr
    join
        $ curry writer <$> renameShadowedVariables' expr
        <*> template @_ @Expr renameShadowedVariables' output
  where
    renameShadowedVariables' = \case
        expr | expr ^. exprMeta % #evaluationStatus == OutOfFuel -> pure expr
        Abstraction
            meta@AbstractionMeta{reified = Just (Hidden reifiedFunction)}
            name
            _ -> do
            suffix <- asks $ freshSuffix name
            let newIdentifier = numberedIdentifier name suffix
            fmap (Abstraction meta newIdentifier . deferEvaluation)
                . newEvalRef
                $ do
                    stubRef <- newIORef . pure $ variable' newIdentifier
                    local (at name ?~ suffix) $ reifiedFunction stubRef
        Abstraction{} -> error
            "Internal evaluation error: object language function was missing meta language function"
        expr -> traversalVL uniplate %%~ renameShadowedVariables'
            =<< traverseOf (exprMeta
                            % #evaluationStatus
                            % _EvaluationDeferred
                            % _Hidden
                            % _2)
                           onDeferredModifier
                           expr
      where
        onDeferredModifier f = do
            fuel <- askLimit
            shadowingEnv <- ask
            pure
                $ usingReaderT shadowingEnv
                . usingLimiterT fuel
                . renameShadowedVariables
                . f

freshSuffix :: Identifier -> Map Identifier Int -> Int
freshSuffix original env
    = Unsafe.fromJust -- safe because suffixRange is infinite
    . getFirst
    $ foldMap (\x -> First . maybe (Just x) (const Nothing)
               $ Map.lookup (numberedIdentifier original x) env) suffixRange
  where
    suffixRange
        = enumFrom . maybe suffixRangeStartDefault succ
        $ Map.lookup original env
    suffixRangeStartDefault
        = if null trailingDigits
            || and ((>) <$> readMaybe trailingDigits <*> Map.lookup trimmed env)
            then 0
            else 1
    (trimmed, trailingDigits)
        = swap
        $ (_Identifier % _UnNonEmpty)
        `passthrough` (swap
                       . second (map unAlphanumeric)
                       . spanEnd (isDigit . unAlphanumeric))
        $ original
