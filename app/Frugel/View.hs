
{-# LANGUAGE RecordWildCards #-}

module Frugel.View where

import Frugel          hiding ( Model(..) )
import Frugel.View.Elements
import Frugel.View.Rendering

import Miso            hiding ( model )
import qualified Miso.String

import Scout
import Scout.Action
import qualified Scout.Internal.Model
import Scout.Model

-- import Text.Show.Pretty ( ppShow )
-- Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model
    = div_ [ codeStyle, class_ "has-background-white-bis" ]
           [ bulmaStyleSheet
           , div_ [ class_ "columns" ]
             $ map (div_ [ class_ "column" ])
                   [ [ instructionsView, editorView model, errorsView model ]
                   , [ evaluatedView model ]
                   ]
           ]

        --    , webPrint $ ppShow model
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

editorView :: Model -> View Action
editorView Model{..}
    = codeRoot [ class_ "block" ]
    . renderDocStream
    . insertCursor cursorOffset
    . layoutPretty defaultLayoutOptions
    $ renderDoc program

errorsView :: Model -> View action
errorsView Model{..}
    = div_ [] . conditionalViews (not $ null errors)
    $ map (pre_ [ class_ "box has-background-danger-light" ]
           . renderDocStream
           . layoutSmart defaultLayoutOptions
           . renderDoc)
          errors

evaluatedView :: Model -> View Action
evaluatedView Model{..}
    = div_
        [ class_ "card" ]
        [ div_ [ class_ "card-header" ]
               [ p_ [ class_ "card-header-title" ]
                    [ if isJust evalThreadId then "Evaluating..." else "Result"
                    ]
               ]
        , div_ [ class_ "card-content" ]
               [ div_ [ class_ "content" ]
                 . renderDocStream
                 . reAnnotateS toStandardAnnotation
                 . layoutPretty defaultLayoutOptions
                 $ unsafePrettyProgram evaluated -- safe because undefined node in top annotation is removed by `reAnnotateS toStandardAnnotation`
               ]
        ]

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
