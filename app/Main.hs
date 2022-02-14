{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Timeout
import Control.ValidEnumerable

import qualified Data.Sequence          as Seq
import Data.Sized

import Frugel
    hiding ( Model, initialModel, updateModel )
import qualified Frugel
import Frugel.Web.Action
import qualified Frugel.Web.Internal.Model
import Frugel.Web.Model
import Frugel.Web.View

import Language.Javascript.JSaddle.Warp.Extra as JSaddleWarp

import Miso                             hiding ( model, node, set, view )
import qualified Miso

import Optics.Extra.Scout

import Scout                            hiding ( Evaluated )

import Test.QuickCheck.Gen

main :: IO ()
main = do
    evalThreadVar <- liftIO $ newMVar Nothing
    -- sadly, handling heap overflows does not work when running in GHCi (because this is not run on the main thread which receives the exception)
    let handleHeapOverflow e = do
            foldMap (flip throwTo e . fst) =<< swapMVar evalThreadVar Nothing
            catchHeapOverflow $ void getLine
        catchHeapOverflow action
            = catchJust (guarded (== HeapOverflow)) action handleHeapOverflow
    catchHeapOverflow $ do
        runApp
            $ startApp App { initialAction = Init
                           , model = initialModel $ programCstrSite' factorial
                           , update = updateModel evalThreadVar
                           , view = viewApp
                           , events = defaultEvents
                           , subs = []
                           , mountPoint = Nothing
                           , logLevel = Off
                           }
        void getLine

updateModel :: MVar (Maybe (ThreadId, Integer))
    -> Action
    -> Model
    -> Effect Action Model
updateModel evalThreadVar Init model = effectSub model $ \sink -> do
    focus "code-root"
    liftIO
        $ reEvaluate evalThreadVar
                     (toFrugelModel model)
                     (model & #editableDataVersion -~ 1)
                     sink
updateModel _ ToggleHelp model = noEff $ #showHelp %~ not $ model
updateModel evalThreadVar GenerateRandom model
    = effectSub model $ \sink -> liftIO $ do
        newProgram <- unSized @500 <.> generate $ uniformValid 500
        let newFrugelModel
                = set #cursorOffset 0
                . snd
                . attemptEdit (const $ Right newProgram) -- reparse the new program for parse errors
                $ toFrugelModel model
        sink . AsyncAction $ NewProgramGenerated newFrugelModel
        reEvaluate evalThreadVar newFrugelModel model sink
updateModel _ (Log msg) model = effectSub model . const . consoleLog $ show msg
updateModel evalThreadVar ToggleLimitEvaluationByDefault model
    = reEvaluateModel evalThreadVar $ #limitEvaluationByDefault %~ not $ model
updateModel evalThreadVar (ChangeFocusedNodeEvaluationIndex indexAction) model
    = effectSub (hideSelectedNodeEvaluation newModel) $ \sink -> liftIO
    . bracketNonTermination (view #editableDataVersion newModel) evalThreadVar
    . yieldWithForcedSelectedNodeEvaluation sink
    $ #evaluationStatus .~ Evaluated
    $ newModel
  where
    newModel
        = model
        & #editableDataVersion +~ 1
        & #selectedNodeEvaluationIndex %~ case indexAction of
            Increment -> min (focusNodeValuesCount - 1) . succ
            Decrement -> max 0 . pred . min (focusNodeValuesCount - 1)
    focusNodeValuesCount
        = Seq.length $ view (#evaluationOutput % #focusedNodeEvaluations) model
updateModel evalThreadVar
            (ChangeFieldRenderDepth field newDepth)
            model@Model{..}
    = if limitEvaluationByDefault
      then noEff $ renderDepthFieldLens field .~ newDepth $ model
      else effectSub
          (hideFieldValues field newModel & #editableDataVersion +~ 1)
          $ \sink -> liftIO
          . bracketNonTermination (view #editableDataVersion model + 1)
                                  evalThreadVar
          $ do
              -- Instead of reevaluating here, it would be more efficient to find and append the extra errors the deeper evaluation produces
              reEvaluatedModel
                  <- fromFrugelModel newModel (toFrugelModel model)
              yieldModel sink $ forceFieldValues field reEvaluatedModel
  where
    newModel = model & renderDepthFieldLens field .~ newDepth
updateModel evalThreadVar (ChangeFuelLimit newLimit) model
    = reEvaluateModel evalThreadVar $ #fuelLimit .~ max 0 newLimit $ model
-- reEvaluate to type error locations
updateModel evalThreadVar PrettyPrint model
    = reEvaluateFrugelModel evalThreadVar
                            (prettyPrint $ toFrugelModel model)
                            model
updateModel evalThreadVar ToggleDefinitionsView model
    = if view #definitionsViewCollapsed model
      then effectSub (hideSelectedNodeDefinitions newModel) $ \sink -> liftIO
          . bracketNonTermination (view #editableDataVersion newModel)
                                  evalThreadVar
          . yieldModel sink
          . forceSelectedNodeDefinitions
          $ #evaluationStatus .~ Evaluated
          $ newModel
      else noEff newModel
  where
    newModel
        = model & #editableDataVersion +~ 1 & #definitionsViewCollapsed %~ not
-- Move action also causes reEvaluation, because value of expression under the cursor may need to be updated
updateModel evalThreadVar (GenericAction genericAction) model
    = case editResult of
        Success -> reEvaluateFrugelModel evalThreadVar newFrugelModel model
        Failure -> noEff $ setFrugelErrors (view #errors newFrugelModel) model
  where
    (editResult, newFrugelModel)
        = Frugel.updateModel genericAction $ toFrugelModel model
updateModel _ (AsyncAction asyncAction) model = case asyncAction of
    EvaluationFinished newModel -> if view #editableDataVersion newModel
        == view #editableDataVersion model
        then noEff newModel
        else effectSub model . const $ pure ()
    NewProgramGenerated frugelModel ->
        noEff $ setWithFrugelModel frugelModel model
    EvaluationAborted msg -> noEff $ #evaluationStatus .~ Aborted msg $ model

reEvaluateModel
    :: MVar (Maybe (ThreadId, Integer)) -> Model -> Effect Action Model
reEvaluateModel evalThreadVar model
    = reEvaluateFrugelModel evalThreadVar (toFrugelModel model) model

reEvaluateFrugelModel :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> Effect Action Model
reEvaluateFrugelModel evalThreadVar frugelModel model
    = effectSub (set #evaluationStatus PartiallyEvaluated
                 $ setWithFrugelModel frugelModel model)
    . (liftIO .)
    $ reEvaluate evalThreadVar frugelModel model

reEvaluate :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> Sink Action
    -> IO ()
reEvaluate
    evalThreadVar
    newFrugelModel
    model@Model{fuelLimit, editableDataVersion, limitEvaluationByDefault}
    sink
    = bracketNonTermination (succ editableDataVersion) evalThreadVar
    $ if limitEvaluationByDefault
      then reportExceptions
          $ yieldWithForcedMainExpression sink =<< partialModel
      else unlessFinishedIn
          500000 -- half a second
          (yieldWithForcedMainExpression sink =<< partialModel)
          . cancelPartialEvaluationOnException
          $ reportExceptions (yieldWithForcedMainExpression sink
                              =<< fromFrugelModel model newFrugelModel)
  where
    partialModel = partialFromFrugelModel (Only fuelLimit) model newFrugelModel
    reportExceptions action
        = catch action (\(e :: SomeException) -> do
                            sink $ AsyncAction $ EvaluationAborted $ show e
                            throwIO e)
    cancelPartialEvaluationOnException action partialThreadVar
        = catch action (\(e :: SomeException) -> do
                            void . traverse killThread
                                =<< tryReadMVar partialThreadVar
                            throwIO e)

yieldWithForcedMainExpression :: Sink Action -> Model -> IO ()
yieldWithForcedMainExpression sink newModel
    = if hasn't #selectedNodeEvaluation newModel
      then yieldModel sink $ forceMainExpression newModel
      else do
          yieldModel sink . forceMainExpression
              $ hideSelectedNodeEvaluation newModel
          yieldWithForcedSelectedNodeEvaluation sink newModel

-- force selected node evaluation separately because evaluation up to a certain depth may encounter non-terminating expressions that were not evaluated in the evaluation of the top expression
yieldWithForcedSelectedNodeEvaluation :: Sink Action -> Model -> IO ()
yieldWithForcedSelectedNodeEvaluation sink newModel
    = if not (contextInView newModel)
      then yieldModel sink $ forceSelectedNodeValue newModel
      else do
          yieldModel sink
              $ forceSelectedNodeValue
              $ hideSelectedNodeContext newModel
          yieldWithForcedSelectedNodeContext sink newModel

yieldWithForcedSelectedNodeContext :: Sink Action -> Model -> IO ()
yieldWithForcedSelectedNodeContext sink newModel
    = if | not $ definitionsInView newModel ->
             yieldModel sink $ forceSelectedNodeVariables newModel
         | not $ variablesInView newModel ->
             yieldModel sink $ forceSelectedNodeDefinitions newModel
         | otherwise -> do
             yieldModel sink . forceSelectedNodeDefinitions
                 $ hideSelectedNodeVariables newModel
             yieldModel sink $ forceSelectedNodeVariables newModel

bracketNonTermination
    :: Integer -> MVar (Maybe (ThreadId, Integer)) -> IO () -> IO ()
bracketNonTermination version evalThreadVar action = do
    threadId <- myThreadId
    outdated <- modifyMVar evalThreadVar $ \v -> case v of
        Just (runningId, runningVersion)
            | version > runningVersion ->
                (Just (threadId, version), False) <$ killThread runningId
        Just _ -> pure (v, True)
        Nothing -> pure (Just (threadId, version), False)
    unless outdated $ do
        u <- action
        void $ swapMVar evalThreadVar $! seq u Nothing

-- yieldModel is strict in the model to make sure evaluation happens in the update thread instead of in the view thread
yieldModel :: Sink Action -> Model -> IO ()
yieldModel sink model = sink . AsyncAction . EvaluationFinished $! model
