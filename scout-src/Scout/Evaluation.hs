{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scout.Evaluation where

import Control.Lens.Plated
import Control.Limited

import Data.Alphanumeric
import Data.Char              ( isDigit )
import Data.Constrained
import Data.Data
import Data.Data.Lens
import Data.Has
import Data.Hidden
import qualified Data.Map     as Map
import Data.MultiSet          ( MultiSet )
import qualified Data.MultiSet as MultiSet

import Frugel                 hiding ( group )

import Optics.Extra.Scout

import Relude                 ( group )
import qualified Relude.Unsafe as Unsafe

import Scout.Internal.EvaluationEnv ( EvaluationEnv(EvaluationEnv) )
import qualified Scout.Internal.EvaluationEnv as EvaluationEnv
import qualified Scout.Internal.Node as Node
import qualified Scout.Internal.Program
import Scout.Node
import Scout.Program
import Scout.Unbound

-- Evaluation output is saved in Meta and is handled manually.
-- It is possible to use a Writer as well, but this can lead to unexpectedly discarded output (due to preserving laziness) and focused node values would need to be handled manually anyway.
type Evaluation = ReaderT EvaluationEnv Limiter

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
            n -> pure $ deferEvaluation n
    pure (errors, newEnv, newCstrSite)

evalExpr :: Expr -> Evaluation Expr
evalExpr expr = do
    ex <- evalExpr' expr
    pure
        $ if expr ^. hasLens @Meta % #focused
          then ex
              & hasLens @Meta % #evaluationOutput % #focusedNodeValues
              %~ (Hidden (ExprNode (ex
                                    & hasLens @Meta % #evaluationOutput
                                    .~ mempty)) <|) -- removing the focused node values from the reported expression prevents double reports due to use of template in collecting reports
          else ex
  where
    -- Just returning v in case it's not in the environment is cool because it frees two beautiful birds with one key: it adds tolerance for binding errors and it makes it possible to print partially applied functions
    evalExpr' v@(Variable _ identifier)
        = fromMaybe variableStub <$> gview (#valueEnv % at identifier)
      where
        variableStub
            = addError (UnboundVariableError identifier)
            $ singleExprNodeCstrSite v
    evalExpr' (Application meta f x) = do
        (efData, ef) <- splitEvaluationOutput <$> evalExpr f
        withEvaluationOutput efData <$> case ef of
            Abstraction
                AbstractionMeta{reified = Just (Hidden reifiedFunction)}
                _
                _ -> fromMaybe applicationStub <$> draw successfulApplication
              where
                successfulApplication = do
                    ex <- evalExpr x
                    Limited <$> magnify #shadowingEnv (reifiedFunction ex)
                applicationStub = addError (OutOfFuelError newApp) placeholder
                placeholder
                    = exprCstrSite' $ fromList [ Right $ ExprNode newApp ]
                -- x is elided because it contains unEvaluated expressions (and renameShadowedVariables needs them evaluated)
                newApp
                    = Application meta ef
                    $ hasLens @Meta % #evaluationStatus .~ OutOfFuel
                    $ x
            Abstraction{} -> error
                "Internal evaluation error: object language function was missing meta language function"
            _ -> withEvaluationOutput (mempty { Node.errors })
                . Application meta checkedEf
                . deferEvaluation
                <$> evalExpr x
              where
                (checkedEf, errors) = reportAnyTypeErrors Function ef
    evalExpr' (Abstraction meta n e) = do
        reifiedFunction <- gviews #valueEnv $ \env x -> do
            ex <- magnify
                (to $ \shadowingEnv ->
                 EvaluationEnv { valueEnv = Map.insert n x env, shadowingEnv })
                $ evalExpr e
            -- each renameShadowedVariables invocation renames all binders, so only the last one's result's is actually used
            renameShadowedVariables ex
        Abstraction (meta & #reified ?~ Hidden reifiedFunction) n
            . deferEvaluation
            -- overwriting n in the environment is important, because otherwise a shadowed variable may be used
            <$> local (chain [ #valueEnv % at n ?~ variable' n
                             , #shadowingEnv % at n ?~ 0
                             ])
                      (evalExpr e)
    -- evalExpr i@(LitN _) = pure i
    evalExpr' (Sum meta x y) = do
        (exData, (ex, exErrors)) <- second (reportAnyTypeErrors Integer)
            . splitEvaluationOutput
            <$> evalExpr x
        (eyData, (ey, eyErrors)) <- second (reportAnyTypeErrors Integer)
            . splitEvaluationOutput
            <$> evalExpr y
        pure
            . withEvaluationOutput
                (mempty { Node.errors = exErrors <> eyErrors })
            . withEvaluationOutput (exData <> eyData)
            $ Sum meta ex ey
        -- case (ex, ey) of
        --     (LitN a, LitN b) -> pure $ LitN (a + b)
        --     _ -> uncurry sum'
        --         <$> traverseOf both (reportAnyTypeErrors Integer) (ex, ey)
    evalExpr' (ExprCstrSite meta cstrSite) = do
        (errors, _, eCstrSite) <- evalCstrSite cstrSite
        pure . withEvaluationOutput (mempty { Node.errors })
            $ ExprCstrSite meta eCstrSite

normaliseExpr :: Expr -> Expr
normaliseExpr
    = transformOf (traverseOf
                   $ traversalVL uniplate
                   `adjoin` (hasLens @Meta
                             % #evaluationOutput
                             % #focusedNodeValues
                             % traversed
                             % _Hidden
                             % traversalVL template))
    $ hasLens @Meta % #evaluationStatus %~ \case
        EvaluationDeferred -> Evaluated
        status -> status

evalWhereClause :: WhereClause -> Evaluation (EvaluationEnv, WhereClause)
evalWhereClause (WhereClause meta decls) = do
    (errors, newEnv) <- evalScope decls
    pure ( newEnv
         , withEvaluationOutput (mempty { Node.errors })
           $ WhereClause meta decls
         )
evalWhereClause (WhereCstrSite meta cstrSite) = do
    (errors, newEnv, eCstrSite) <- evalCstrSite cstrSite
    pure ( newEnv
         , withEvaluationOutput (mempty { Node.errors })
           $ WhereCstrSite meta eCstrSite
         )

-- todo: return evaluated decls
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
    computeNewEnv env@EvaluationEnv{..}
        = flip (set #valueEnv) withUpdatedShadowingEnv <$> newValueEnv
      where
        withUpdatedShadowingEnv
            = env
            & #shadowingEnv
            .~ shadowingEnv
            <> fromList (zip (decls ^.. folded % #name) $ repeat 0)
        newValueEnv
            = mappend valueEnv . fromList
            <$> traverse evalDecl
                         (decls ^.. folded % ((,) <$^> #name <*^> #value))
        evalDecl (name, value)
            = (name, ) . fromMaybe evaluationStub <$> draw successfulEvaluation
          where
            successfulEvaluation = do
                iteratedEnv <- newValueEnv
                Limited
                    <.> usingReaderT
                        (withUpdatedShadowingEnv & #valueEnv .~ iteratedEnv)
                    $ evalExpr value
            evaluationStub
                = addError (OutOfFuelError $ variable' name) . exprCstrSite'
                $ fromList [ Right . ExprNode $ variable' name ]

evalProgram :: Program -> Evaluation Program
evalProgram Program{..} = do
    eWhereClause <- traverse evalWhereClause whereClause
    (newEnv, eWhereClauseData) <- asks $ \env -> maybe
        (env, mempty)
        (second . view $ hasLens @Meta % #evaluationOutput)
        eWhereClause
    newExpr <- local (const newEnv) $ evalExpr expr
    pure Program { meta = meta
                       & #standardMeta
                       %~ chain [ #interstitialWhitespace .~ [ "" ]
                                , #evaluationOutput .~ eWhereClauseData
                                ]
                 , expr = newExpr
                 , whereClause = Nothing
                 }
evalProgram (ProgramCstrSite meta cstrSite) = do
    (errors, _, eCstrSite) <- evalCstrSite cstrSite
    pure . withEvaluationOutput (mempty { Node.errors })
        $ ProgramCstrSite meta eCstrSite

-- fuel is used at applications and recursion and specifies a depth of the computation rather than a length
runEval :: (Data a, Decomposable a, NodeOf a ~ Node, Unbound a, Has Meta a)
    => Maybe Int
    -> Limit
    -> (a -> Evaluation a)
    -> a
    -> (a, (MultiSet EvaluationError, Seq Node))
runEval cursorOffset fuel eval x
    = removeReifiedFunctions
    . (removeEvaluationOutput
       &&& (MultiSet.fromOccurMap
            . removeEvaluationOutput
            . MultiSet.toMap -- unwrap and rewrap MultiSet newtype because of Data MultiSet instance hiding it's contents to optics
            . view #errors
            &&& view (#focusedNodeValues % mapping _Hidden))
       . collectEvaluationOutput)
    . normaliseAllExpressions
    . usingLimiter fuel
    . usingReaderT (mempty { EvaluationEnv.shadowingEnv = Map.fromSet (const 0)
                                 $ freeVariables mempty x
                           })
    . eval
    $ maybe id focusNodeUnderCursor cursorOffset x
  where
    normaliseAllExpressions = traversalVL (template @_ @Expr) %~ normaliseExpr
    removeReifiedFunctions
        = traversalVL (template @_ @AbstractionMeta) % #reified .~ Nothing
    removeEvaluationOutput :: Data a => a -> a
    removeEvaluationOutput
        = traversalVL (template @_ @Meta) % #evaluationOutput .~ mempty
    collectEvaluationOutput :: (Has Meta a, Data a) => a -> EvaluationOutput
    collectEvaluationOutput
        = foldOf (allEvaluated
                  % _UnConstrained (castOptic @A_Getter $ hasLens @Meta)
                  % #evaluationOutput)

splitEvaluationOutput :: Has Meta n => n -> (EvaluationOutput, n)
splitEvaluationOutput = hasLens @Meta % #evaluationOutput <<.~ mempty

addError :: Has Meta n => EvaluationError -> n -> n
addError e
    = withEvaluationOutput (mempty { Node.errors = MultiSet.singleton e })

withEvaluationOutput :: Has Meta n => EvaluationOutput -> n -> n
withEvaluationOutput output = hasLens @Meta % #evaluationOutput %~ (output <>)

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

renameShadowedVariables :: Expr -> ReaderT ShadowingEnv Limiter Expr
renameShadowedVariables = \case
    Abstraction
        absMeta@AbstractionMeta{reified = Just (Hidden reifiedFunction)}
        name
        _ -> do
        newMeta <- absMeta
            & hasLens @Meta
            % #evaluationOutput
            % (#errors % traversalVL template `adjoin` focusedExprs)
            %%~ renameShadowedVariables
        suffix <- asks $ freshSuffix name
        let newIdentifier = numberedIdentifier name suffix
        Abstraction newMeta newIdentifier . deferEvaluation
            <$> local (at name ?~ suffix)
                      (reifiedFunction $ variable' newIdentifier)
    Abstraction{} -> error
        "Internal evaluation error: object language function was missing meta language function"
    expr -> expr
        & traversalVL uniplate
        `adjoin` (hasLens @Meta % #evaluationOutput % focusedExprs)
        %%~ renameShadowedVariables
  where
    focusedExprs
        = #focusedNodeValues % traversed % _Hidden % traversalVL template

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
