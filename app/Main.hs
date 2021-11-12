{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.ValidEnumerable

import qualified Data.Sequence          as Seq
import Data.Sized

import Frugel
    hiding ( Model, initialModel, updateModel )
import qualified Frugel
import Frugel.View

import Language.Javascript.JSaddle.Warp.Extra as JSaddleWarp

import Miso                             hiding ( model, node, set, view )
import qualified Miso

import Optics.Extra.Scout

import Scout
import Scout.Action
import qualified Scout.Internal.Model
import Scout.Model

import Test.QuickCheck.Gen

-- Entry point for a miso application
main :: IO ()
main = runApp $ do
    evalThreadVar <- liftIO $ newMVar Nothing
    startApp
        App { initialAction = Init  -- initial action to be executed on application load
            , model = initialModel $ programCstrSite' evalTest  -- initial model
            , update = updateModel evalThreadVar -- update function
            , view = viewModel -- view function
            , events = defaultEvents -- default delegated events
            , subs = [] -- empty subscription list
            , mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
            , logLevel = Off -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)
            }

updateModel :: MVar (Maybe (ThreadId, Integer))
    -> Action
    -> Model
    -> Effect Action Model
updateModel evalThreadVar action model'
    = either id (effectSub model') $ updateModel' action model'
  where
    updateModel' Init model = Right $ \sink -> do
        focus "code-root"
        liftIO
            $ reEvaluate evalThreadVar
                         (toFrugelModel model)
                         (model & #editableDataVersion -~ 1)
                         sink
    updateModel' GenerateRandom model = Right $ \sink -> liftIO $ do
        newProgram <- unSized @500 <.> generate $ uniformValid 500
        let newFrugelModel
                = set #cursorOffset 0
                . snd
                . attemptEdit (const $ Right newProgram) -- reparse the new program for parse errors
                $ toFrugelModel model
        sink . AsyncAction $ NewProgramGenerated newFrugelModel
        reEvaluate evalThreadVar newFrugelModel model sink
    updateModel' (Log msg) _ = Right . const . consoleLog $ show msg
    updateModel' (ChangeFocusedNodeEvaluationIndex indexAction) model
        = Left . effectSub (hideSelectedNodeEvaluation newModel) $ \sink ->
        liftIO
        . bracketNonTermination (view #editableDataVersion newModel)
                                evalThreadVar
        . yieldWithForcedSelectedNodeEvaluation sink
        $ #partiallyEvaluated .~ False
        $ newModel
      where
        newModel
            = model
            & #editableDataVersion +~ 1
            & #selectedNodeEvaluationIndex %~ case indexAction of
                Increment -> min (focusNodeValuesCount - 1) . succ
                Decrement -> max 0 . pred . min (focusNodeValuesCount - 1)
        focusNodeValuesCount
            = Seq.length
            $ view (#evaluationOutput % #focusedNodeEvaluations) model
    updateModel' (ChangeFieldRenderDepth field newDepth) model
        = if field == Value || contextInView model
          then Left
              . effectSub
                  (hideFieldValues field newModel & #editableDataVersion +~ 1)
              $ \sink -> liftIO
              . bracketNonTermination (view #editableDataVersion model + 1)
                                      evalThreadVar
              $ do
                  reEvaluatedModel
                      <- fromFrugelModel newModel (toFrugelModel model)
                  yieldModel sink $ forceFieldValues field reEvaluatedModel
          else Left $ noEff $ renderDepthFieldLens field .~ newDepth $ model
      where
        newModel
            = model
            & #partiallyEvaluated .~ True
            & renderDepthFieldLens field .~ newDepth
    updateModel' (ChangeFuelLimit newLimit) model
        = Left . reEvaluateModel evalThreadVar
        $ #fuelLimit .~ max 0 newLimit
        $ model
    -- reEvaluate to type error locations
    updateModel' PrettyPrint model
        = Left
        $ reEvaluateFrugelModel evalThreadVar
                                (prettyPrint $ toFrugelModel model)
                                model
    updateModel' ToggleDefinitionsView model
        = if view #definitionsViewCollapsed model
          then Left . effectSub (hideSelectedNodeDefinitions newModel)
              $ \sink -> liftIO
              . bracketNonTermination (view #editableDataVersion newModel)
                                      evalThreadVar
              . yieldModel sink
              . forceSelectedNodeDefinitions
              $ #partiallyEvaluated .~ False
              $ newModel
          else Left $ noEff newModel
      where
        newModel
            = model
            & #editableDataVersion +~ 1
            & #definitionsViewCollapsed %~ not
    -- Move action also causes reEvaluation, because value of expression under the cursor may need to be updated
    updateModel' (GenericAction genericAction)
                 model = Left $ case editResult of
        Success -> reEvaluateFrugelModel evalThreadVar newFrugelModel model
        Failure -> noEff
            $ updateWithFrugelErrors (view #errors newFrugelModel) model
      where
        (editResult, newFrugelModel)
            = Frugel.updateModel genericAction $ toFrugelModel model
    updateModel' (AsyncAction asyncAction) model
        = if view #editableDataVersion newModel
              == view #editableDataVersion model
          then Left $ noEff newModel
          else Right . const $ pure ()
      where
        newModel = case asyncAction of
            EvaluationFinished m -> m
            NewProgramGenerated frugelModel ->
                updateWithFrugelModel frugelModel model

reEvaluateModel
    :: MVar (Maybe (ThreadId, Integer)) -> Model -> Effect Action Model
reEvaluateModel evalThreadVar model
    = reEvaluateFrugelModel evalThreadVar (toFrugelModel model) model

reEvaluateFrugelModel :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> Effect Action Model
reEvaluateFrugelModel evalThreadVar frugelModel model
    = effectSub (updateWithFrugelModel frugelModel model) . (liftIO .)
    $ reEvaluate evalThreadVar frugelModel model

reEvaluate :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> Sink Action
    -> IO ()
reEvaluate evalThreadVar newFrugelModel model@Model{fuelLimit} sink = do
    partialModel@Model{editableDataVersion}
        <- partialFromFrugelModel (Only fuelLimit) model newFrugelModel
    -- safe because evaluation had fuel limit
    yieldModel sink $ forceMainExpression partialModel
    bracketNonTermination (succ editableDataVersion) evalThreadVar
        -- forcing is safe because inside bracketNonTermination
        $ yieldWithForcedMainExpression sink
        =<< fromFrugelModel model newFrugelModel

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
