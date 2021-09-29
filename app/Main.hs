{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.ValidEnumerable

import Data.Composition
import Data.Data.Lens
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

makePrisms ''Effect

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

updateModel :: MVar (Maybe ThreadId) -> Action -> Model -> Effect Action Model

-- If the action causes a pure update to the model, increment the version, but don't change it otherwise
updateModel evalThreadVar action model'
    = either (_Effect % _1 % #version %~ succ) (effectSub model')
    $ updateModel' action model'
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
        sink . AsyncAction 1 $ NewProgramGenerated preEvaluationModel
        sub sink
    updateModel' (Log msg) _ = Right . const . consoleLog $ show msg
    updateModel' (FocusedNodeValueIndexAction indexAction) model
        = Left . effectSub (hideSelectedNodeValue newModel) $ \sink -> liftIO
        . bracketNonTermination evalThreadVar
        . unsafeEvaluateSelectedNodeValue sink
        $ #partiallyEvaluated .~ False
        $ newModel
      where
        newModel = model & #focusedNodeValueIndex %~ case indexAction of
            Increment -> min
                (lengthOf (#evaluationOutput % #focusedNodeValues % folded)
                          model
                 - 1)
                . succ
            Decrement -> max 0 . pred
    updateModel' (ChangeFuelLimit newLimit) model
        = Left . reEvaluateModel evalThreadVar
        $ model & #fuelLimit .~ max 0 newLimit
    -- reEvaluate to update type error locations
    updateModel' PrettyPrint model
        = Left
        $ reEvaluateFrugelModel evalThreadVar
                                (prettyPrint $ toFrugelModel model)
                                model
    -- Move action also cause reEvaluation, because value of expression under the cursor may need to be updated
    updateModel' (GenericAction genericAction)
                 model = Left $ case editResult of
        Success -> uncurry effectSub . over _2 (liftIO .)
            $ reEvaluate evalThreadVar newFrugelModel model
        Failure -> noEff
            $ updateWithFrugelErrors (view #errors newFrugelModel) model
      where
        (editResult, newFrugelModel)
            = Frugel.updateModel genericAction $ toFrugelModel model
    updateModel' (AsyncAction versionIncrement asyncAction) model
        = if view #version newModel > view #version model
          then Left . noEff
              $ #version -~ 1
              $ newModel -- newModel version was already higher, don't increment again in updateModel
          else Right . const $ pure ()
      where
        newModel = #version +~ toInteger versionIncrement $ case asyncAction of
            EvaluationFinished m -> m
            NewProgramGenerated m -> m

reEvaluateModel :: MVar (Maybe ThreadId) -> Model -> Effect Action Model
reEvaluateModel evalThreadVar model
    = reEvaluateFrugelModel evalThreadVar (toFrugelModel model) model

reEvaluateFrugelModel :: MVar (Maybe ThreadId)
    -> Frugel.Model Program
    -> Model
    -> Effect Action Model
reEvaluateFrugelModel = uncurry effectSub . over _2 (liftIO .) .:. reEvaluate

reEvaluate :: MVar (Maybe ThreadId)
    -> Frugel.Model Program
    -> Model
    -> ( Model
       , Sink Action
         -> IO ()
       )
reEvaluate evalThreadVar newFrugelModel model@Model{fuelLimit}
    = (partialFromFrugelModel (Only fuelLimit) model newFrugelModel, \sink ->
    bracketNonTermination evalThreadVar $ do
        let newModel = unsafeFromFrugelModel model newFrugelModel
        -- safe because inside bracketNonTermination
        unsafeEvaluateTopExpression sink newModel
        -- force selected node value separately because evaluation up to a certain depth may encounter non-terminating expressions that were not evaluated in the evaluation of the top expression
        unsafeEvaluateSelectedNodeValue sink
            $ #version +~ 1
            $ #partiallyEvaluated .~ False
            $ newModel)

bracketNonTermination :: MVar (Maybe ThreadId) -> IO () -> IO ()
bracketNonTermination evalThreadVar action = do
    threadId <- myThreadId
    modifyMVar_ evalThreadVar $ \i -> Just threadId <$ traverse killThread i
    action
    void $ swapMVar evalThreadVar Nothing

-- force errors to force full evaluation
unsafeEvaluateTopExpression :: Sink Action -> Model -> IO ()
unsafeEvaluateTopExpression sink model@Model{..}
    = seq (length errors) . sink . AsyncAction 2 . EvaluationFinished
    $ hideSelectedNodeValue model

unsafeEvaluateSelectedNodeValue :: Sink Action -> Model -> IO ()
unsafeEvaluateSelectedNodeValue sink model
    = seq (lengthOf (#selectedNodeValue
                     % to (capTree 10)
                     % traversalVL (template @_ @Identifier))
                    model)
    . sink
    . AsyncAction 2
    $ EvaluationFinished model
