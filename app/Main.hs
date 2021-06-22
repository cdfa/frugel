{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Frugel
import Frugel.View
import Frugel.View.Elements

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp as JSaddle
#endif

import Miso

import Scout

import Text.Show.Pretty

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
    initialAction = Load  -- initial action to be executed on application load
    model = initialModel $ programCstrSite' whereClauseTest  -- initial model
    update = updateModel -- update function
    view = viewModel -- view function
    events = defaultEvents -- default delegated events
    subs = [] -- empty subscription list
    mountPoint = Nothing -- mount point for application (Nothing defaults to 'body')
    logLevel = DebugPrerender -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)

-- Constructs a virtual DOM from a model
viewModel :: Model Program -> View Action
viewModel model
    = div_ [ codeStyle, class_ "has-background-white-bis" ]
           [ link_ [ rel_ "stylesheet"
                   , href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
                   ]
           , div_ [ class_ "columns" ]
             $ map (div_ [ class_ "column" ])
                   [ [ codeRoot []
                       . renderSmart
                       . insertCursor (cursorOffset model)
                       . layoutSmart defaultLayoutOptions
                       . renderDoc
                       $ program model
                     ]
                   , conditionalViews (not . null $ errors model)
                     $ map (pre_ [ class_ "box has-background-danger-light" ]
                            . renderSmart
                            . layoutSmart defaultLayoutOptions
                            . renderDoc)
                     $ errors model
                   ]
           , webPrint $ ppShow model
           ]
