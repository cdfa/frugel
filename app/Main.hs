{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.ValidEnumerable

import Data.Sized

import Frugel                     hiding ( updateModel )
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
            , model         = initialModel $ programCstrSite' whereClauseTest  -- initial model
            , update        = updateModel -- update function
            , view          = viewModel -- view function
            , events        = defaultEvents -- default delegated events
            , subs          = [] -- empty subscription list
            , mountPoint    = Nothing -- mount point for application (Nothing defaults to 'body')
            , logLevel      = DebugPrerender -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)
            }

updateModel :: Action -> Model Program -> Effect Action (Model Program)
updateModel Init model = fromTransition (scheduleIO_ $ focus "code-root") model
updateModel GenerateRandom model = model <# do
    NewModel . flip (set #program) model
        <$> (liftIO . unSized <.> generate @(Sized 500 Program)
             $ uniformValid 500)
updateModel (NewModel model) _ = noEff model
updateModel (Log msg) model
    = fromTransition (scheduleIO_ . consoleLog $ show msg) model
updateModel PrettyPrint model = noEff $ prettyPrint model
updateModel (GenericAction action) model
    = noEff $ Frugel.updateModel action model
