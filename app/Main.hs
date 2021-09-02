{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent
import Control.ValidEnumerable

import Data.Sized

import Frugel                     hiding ( Model, initialModel, updateModel )
import qualified Frugel
import Frugel.View

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
#endif

import Miso                       hiding ( model, node, set, view )
import qualified Miso

import Optics.Extra

import Scout
import Scout.Action
import qualified Scout.Internal.Model
import Scout.Model

import Test.QuickCheck.Gen

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp = JSaddle.debug 3708

#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- Entry point for a miso application
main :: IO ()
main
    = runApp
    $ startApp
        App { initialAction = Init  -- initial action to be executed on application load
            , model = initialModel $ programCstrSite' evalTest  -- initial model
            , update = updateModel -- update function
            , view = viewModel -- view function
            , events = defaultEvents -- default delegated events
            , subs = [] -- empty subscription list
            , mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
            , logLevel = DebugPrerender -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)
            }

updateModel :: Action -> Model -> Effect Action Model
updateModel Init model = fromTransition (scheduleIO_ $ focus "code-root") model
-- updateModel GenerateRandom model = model <# do
--     liftIO $ threadDelay (5 * 10 ^ 6)
--     pure . ModifyModel $ const model
-- updateModel GenerateRandom model = effectSub model $ \sink -> do
--     void . liftIO . forkIO
--         $ (threadDelay (5 * 10 ^ 6) >> sink (NewModel model))
updateModel GenerateRandom model = effectSub model $ \sink -> liftIO $ do
    newProgram <- unSized @500 <.> generate $ uniformValid 500
    let (preEvaluationModel, sub)
            = reEvaluate
                (set #cursorOffset 0
                 . snd
                 . attemptEdit (const $ Right newProgram)) -- reparse the new program for parse errors
                model
    sink . ModifyModel $ const preEvaluationModel
    sub sink
updateModel (ModifyModel f) model = noEff $ f model
updateModel (Log msg) model
    = fromTransition (scheduleIO_ . consoleLog $ show msg) model
-- reEvaluate to update type error locations
updateModel PrettyPrint model
    = uncurry effectSub . over _2 (liftIO .) $ reEvaluate prettyPrint model
updateModel (GenericAction action@(Move _)) model
    = noEff
    $ set #cursorOffset
          (view #cursorOffset . snd . Frugel.updateModel action
           $ toFrugelModel model)
          model
updateModel (GenericAction action) model = case editResult of
    Success -> uncurry effectSub . over _2 (liftIO .)
        $ reEvaluate (const newFrugelModel) model
    Failure -> noEff
        $ updateWithFrugelErrors (view #errors newFrugelModel) model
  where
    (editResult, newFrugelModel)
        = Frugel.updateModel action $ toFrugelModel model

reEvaluate :: (Frugel.Model Program -> Frugel.Model Program)
    -> Model
    -> ( Model
       , Sink Action
         -> IO ()
       )
reEvaluate f model@Model{evalThreadId, fuelLimit}
    = ( set #evalThreadId Nothing
        $ partialFromFrugelModel (Only initialFuelLimit)
                                 fuelLimit
                                 newFrugelModel
      , \sink -> do
            traverse_ killThread evalThreadId
            threadId <- myThreadId
            sink . ModifyModel $ set #evalThreadId (Just threadId)
            let newModel@Model{errors}
                    = unsafeFromFrugelModel fuelLimit newFrugelModel
            seq (length errors) . sink . ModifyModel $ const newModel -- force errors to force all expressions
      )
  where
    newFrugelModel = f . set #errors [] $ toFrugelModel model
