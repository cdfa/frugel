{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
#endif

import           Miso
import qualified Miso.String
import           Frugel
import           Text.Pretty.Simple               ( pShowNoColor )
import           Prettyprinter

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp = JSaddle.debug 3708

#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- Entry point for a miso application
main :: IO ()
main = runApp $ startApp App { .. }
  where
    initialAction = NoOp  -- initial action to be executed on application load
    model = initialModel  -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = DebugPrerender -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel
    = div_ []
    . flap -- apply the function in the list to the model
        [ webPrint . pShowNoColor
        , webPrint . renderSmart . prettyHoleContents
        , either
              (webPrint . pShowNoColor)
              (webPrint . renderSmart . prettyNode)
          . parseHole "notepad"
        ]

webPrint :: Miso.String.ToMisoString a => a -> View Action
webPrint x = pre_ [] [ text $ Miso.String.ms x ]

renderSmart :: Doc HoleAnnotation -> Text
renderSmart = renderHoleAnnotation . layoutSmart defaultLayoutOptions
