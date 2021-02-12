{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
#endif

import           Miso
import           Miso.String
import           Frugel

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
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model = 0 -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = DebugPrerender -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel x =
    div_
        []
        [ button_ [ onClick AddOne ] [ text "+" ]
        , text (ms x)
        , button_ [ onClick SubtractOne ] [ text "-" ]
        ]
