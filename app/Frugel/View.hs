module Frugel.View where

import Frugel
import Frugel.View.Elements
import Frugel.View.Rendering

import Miso            hiding ( model )
import qualified Miso.String

import Scout
import Scout.Action

import Text.Show.Pretty ( ppShow )

-- Constructs a virtual DOM from a model
viewModel :: Model Program -> View Action
viewModel model
    = div_ [ codeStyle, class_ "has-background-white-bis" ]
           [ bulmaStyleSheet
           , div_ [ class_ "columns" ]
             $ map (div_ [ class_ "column" ])
                   [ [ instructionsView, editorView model, errorsView model ]
                   , [ evaluatedView model, webPrint $ ppShow model ]
                   ]
           ]

bulmaStyleSheet :: View action
bulmaStyleSheet
    = link_ [ rel_ "stylesheet"
            , href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
            ]

instructionsView :: View Action
instructionsView
    = div_ [ class_ "box" ]
           [ text "Type as usual, use arrow keys to move"
           , div_ [ class_ "buttons" ]
                  [ button_ [ onClick PrettyPrint, class_ "button" ]
                            [ text "Format" ]
                  , button_ [ onClick GenerateRandom, class_ "button" ]
                            [ text "Generate Random" ]
                  ]
           ]

editorView :: Model Program -> View Action
editorView model
    = codeRoot []
    . renderSmart
    . insertCursor (cursorOffset model)
    . layoutPretty defaultLayoutOptions
    . renderDoc
    $ program model

errorsView :: Model Program -> View action
errorsView model
    = div_ []
    $ conditionalViews (not . null $ errors model)
    $ map (pre_ [ class_ "box has-background-danger-light" ]
           . renderSmart
           . layoutSmart defaultLayoutOptions
           . renderDoc)
    $ errors model

evaluatedView :: Model Program -> View Action
evaluatedView model = div_ [] [ text "TBD" ]

    -- . renderSmart
    -- . layoutPretty defaultLayoutOptions
    -- . renderDoc
    -- . fst
    -- . runEval
    -- . program
webPrint :: Miso.String.ToMisoString a => a -> View action
webPrint x = pre_ [] [ text $ Miso.String.ms x ]

insertCursor :: Int -> SimpleDocStream Annotation -> SimpleDocStream Annotation
insertCursor 0 s = SAnnPush Frugel.Cursor $ SAnnPop s
insertCursor offset s = case s of
    SFail -> error "Encountered SFail in DocStream"
    SEmpty -> error
        ("offset " <> show offset <> " was out of bounds for the DocStream")
    (SChar c s') -> SChar c $ insertCursor (offset - 1) s'
    (SText len txt s')
        | offset > len -> SText len txt $ insertCursor (offset - len) s'
    (SText _ txt s') -> insertCursor offset . foldr SChar s' $ toString txt
    (SLine nextLineIndent s')
        | offset > 1 + nextLineIndent -> SLine nextLineIndent
            $ insertCursor (offset - 1 - nextLineIndent) s'
    (SLine nextLineIndent s') -> SLine 0
        $ insertCursor (offset - 1)
        $ SText nextLineIndent (toText $ replicate nextLineIndent ' ') s'
    (SAnnPush ann s') -> SAnnPush ann $ insertCursor offset s'
    (SAnnPop s') -> SAnnPop $ insertCursor offset s'
