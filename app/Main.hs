{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.ValidEnumerable

import Data.Composition
import Data.Data.Lens
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
    updateModel' Init _ = Right . const $ focus "code-root"
    updateModel' GenerateRandom model = Right $ \sink -> liftIO $ do
        newProgram <- unSized @500 <.> generate $ uniformValid 500
        let newFrugelModel
                = set #cursorOffset 0
                . snd
                . attemptEdit (const $ Right newProgram) -- reparse the new program for parse errors
                $ toFrugelModel model
        let (preEvaluationModel, sub)
                = reEvaluate evalThreadVar newFrugelModel model
        sink . AsyncAction $ NewProgramGenerated preEvaluationModel
        sub sink
    updateModel' (Log msg) _ = Right . const . consoleLog $ show msg
    updateModel' (FocusedNodeValueIndexAction indexAction) model
        = Left . effectSub (hideSelectedNodeValue newModel) $ \sink -> liftIO
        . bracketNonTermination (view #editableDataVersion newModel)
                                evalThreadVar
        . unsafeEvaluateSelectedNodeValue sink
        $ #partiallyEvaluated .~ False
        $ newModel
      where
        newModel
            = model
            & #editableDataVersion +~ 1
            & #focusedNodeValueIndex %~ case indexAction of
                Increment -> min (focusNodeValuesCount - 1) . succ
                Decrement -> max 0 . pred . min (focusNodeValuesCount - 1)
        focusNodeValuesCount
            = Seq.length $ view (#evaluationOutput % #focusedNodeValues) model
    updateModel' (ChangeFuelLimit newLimit) model
        = Left . reEvaluateModel evalThreadVar
        $ model & #fuelLimit .~ max 0 newLimit
    -- reEvaluate to update type error locations
    updateModel' PrettyPrint model
        = Left
        $ reEvaluateFrugelModel evalThreadVar
                                (prettyPrint $ toFrugelModel model)
                                model
    -- Move action also causes reEvaluation, because value of expression under the cursor may need to be updated
    updateModel' (GenericAction genericAction)
                 model = Left $ case editResult of
        Success -> uncurry effectSub . over _2 (liftIO .)
            $ reEvaluate evalThreadVar newFrugelModel model
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
            NewProgramGenerated m -> m

reEvaluateModel
    :: MVar (Maybe (ThreadId, Integer)) -> Model -> Effect Action Model
reEvaluateModel evalThreadVar model
    = reEvaluateFrugelModel evalThreadVar (toFrugelModel model) model

reEvaluateFrugelModel :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> Effect Action Model
reEvaluateFrugelModel = uncurry effectSub . over _2 (liftIO .) .:. reEvaluate

reEvaluate :: MVar (Maybe (ThreadId, Integer))
    -> Frugel.Model Program
    -> Model
    -> ( Model
       , Sink Action
         -> IO ()
       )
reEvaluate evalThreadVar
           newFrugelModel
           model@Model{fuelLimit, editableDataVersion}
    = (partialFromFrugelModel (Only fuelLimit) model newFrugelModel, \sink ->
    bracketNonTermination (succ editableDataVersion) evalThreadVar $ do
        let newModel = fromFrugelModel model newFrugelModel
        -- safe because inside bracketNonTermination
        unsafeEvaluateTopExpression sink newModel
        -- force selected node value separately because evaluation up to a certain depth may encounter non-terminating expressions that were not evaluated in the evaluation of the top expression
        unsafeEvaluateSelectedNodeValue sink
            $ #partiallyEvaluated .~ False
            $ newModel)

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
        void $ swapMVar evalThreadVar $ seq u Nothing

-- force errors to force full evaluation
unsafeEvaluateTopExpression :: Sink Action -> Model -> IO ()
unsafeEvaluateTopExpression sink model@Model{..}
    = seq (length errors) . sink . AsyncAction . EvaluationFinished
    $ hideSelectedNodeValue model

unsafeEvaluateSelectedNodeValue :: Sink Action -> Model -> IO ()
unsafeEvaluateSelectedNodeValue sink model
    = seq (lengthOf (#selectedNodeValue
                     % to (capTree 10)
                     % traversalVL (template @_ @Identifier))
                    model)
    . sink
    . AsyncAction
    $ EvaluationFinished model
