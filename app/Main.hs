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
import           View
import           View.Elements
import           Test

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
viewModel model
    = div_
        [ codeStyle, class_ "has-background-white-bis" ]
        [ link_
              [ rel_ "stylesheet"
              , href_
                    "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
              ]
          --   , renderSmart test2
        , div_ [ class_ "columns" ] . map (div_ [ class_ "column" ] . one)
          $ flap -- apply the function in the list to the model
              [ renderSmart . prettyHoleContents
              , either webPrint (renderSmart . prettyProgram)
                . parseHole "notepad"
              ]
              model
        , webPrint $ pShowNoColor model
        ]

webPrint :: Miso.String.ToMisoString a => a -> View Action
webPrint x = pre_ [] [ text $ Miso.String.ms x ]
-- test :: Doc Annotation
-- test
--     = let
--         prettyType
--             = align
--             . sep
--             . zipWith (<+>) ("::" : repeat "->")
--             . map PrettyPrinting.node
--         prettySig name ty
--             = PrettyPrinting.node
--                 (PrettyPrinting.node (pretty name) <+> prettyType ty)
--         in
--             PrettyPrinting.inHole
--             $ prettySig
--                 ("ex   ample" :: Text)
--                 [ PrettyPrinting.outOfHole "Int", "Bool", "Char", "IO ()" ]
-- test2 :: Doc Annotation
-- test2
--     = PrettyPrinting.inHole
--         ("outS0"
--          <> PrettyPrinting.outOfHole
--              ("inS1" <> PrettyPrinting.inHole (line <> "x" <> line) <> "inE1")
--          <> "outE0")
