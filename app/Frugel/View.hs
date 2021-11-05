{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Frugel.View where

import qualified Data.Sequence as Seq

import Frugel          hiding ( Model(..) )
import Frugel.View.Elements
import Frugel.View.Rendering

import Miso            hiding ( model, set, view )
import qualified Miso.String as Miso

import Optics.Extra.Scout

import Scout
import Scout.Action
import qualified Scout.Internal.Model
import Scout.Model

-- import Text.Show.Pretty ( ppShow )
-- Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel model
    = div_
        [ class_ "code has-background-white-bis" ]
        [ styleSheet "bulma.min.css"
        , styleSheet "style.css"
        , div_ [ class_ "columns" ]
          $ map
              (div_ [ class_ "column" ])
              [ [ instructionsView model, editorView model, errorsView model ]
              , [ evaluatedView model ]
              ]
        ]

        --    , webPrint $ ppShow model
styleSheet :: Miso.MisoString -> View action
styleSheet path = link_ [ rel_ "stylesheet", href_ path ]

instructionsView :: Model -> View Action
instructionsView Model{..}
    = div_ [ class_ "box" ]
           [ text "Type as usual, use arrow keys to move"
           , br_ []
           , text "Fuel limit for evaluation following each keystroke "
           , input_ [ type_ "number"
                    , value_ . Miso.ms $ show @String fuelLimit
                    , onChange (\value -> ChangeFuelLimit
                                    (fromMaybe fuelLimit . readMaybe
                                     $ Miso.fromMisoString value))
                    ]
           , div_ [ class_ "buttons" ]
                  [ button_ [ onClick PrettyPrint, class_ "button" ]
                            [ text "Format" ]
                  , button_ [ onClick GenerateRandom, class_ "button" ]
                            [ text "Generate Random" ]
                  ]
           ]

editorView :: Model -> View Action
editorView Model{..}
    = codeRoot []
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
evaluatedView model@Model{..}
    = div_ [ class_ "card" ]
    $ [ div_ [ class_ "card-header" ]
             [ p_ [ class_ "card-header-title" ]
                  [ if partiallyEvaluated then "Evaluating..." else "Result" ]
             ]
      , div_ [ class_ "card-header" ]
             [ p_ [ class_ "card-header-title" ] [ "Full program" ] ]
      , div_ [ class_ "card-content" ]
             [ div_ [ class_ "content" ]
               . renderDocStream
               . reAnnotateS toStandardAnnotation
               . layoutPretty defaultLayoutOptions
               . unsafePrettyProgram
               $ view #evaluated evaluationOutput -- safe because undefined node in top annotation is removed by `reAnnotateS toStandardAnnotation`
             ]
      ]
    ++ foldMapOf
        #selectedNodeValue
        (\selectedNodeValue ->
         [ div_ [ class_ "card-header" ]
                [ p_ [ class_ "card-header-title" ]
                     [ "Focused node value up to depth: "
                     , input_ [ type_ "number"
                              , value_ . Miso.ms
                                $ show @String selectedNodeValueRenderDepth
                              , onChange (\value ->
                                          ChangeSelectedNodeValueRenderDepth
                                              (fromMaybe fuelLimit . readMaybe
                                               $ Miso.fromMisoString value))
                              ]
                     ]
                , button_ [ class_ "card-header-icon"
                          , onClick (FocusedNodeValueIndexAction Decrement)
                          ]
                          [ span_ [ class_ "icon" ] [ text "ᐊ" ] ]
                , span_ [ class_ "card-header-vertical-padding" ]
                        [ let focusedNodeValuesCount
                                  = Seq.length
                                  $ view #focusedNodeValues evaluationOutput
                          in text
                             $ show (min focusedNodeValuesCount
                                         (focusedNodeValueIndex + 1))
                             <> " of "
                             <> show focusedNodeValuesCount
                        ]
                , button_ [ class_ "card-header-icon"
                          , onClick (FocusedNodeValueIndexAction Increment)
                          ]
                          [ span_ [ class_ "icon" ] [ text "ᐅ" ] ]
                ]
         , div_ [ class_ "card-content" ]
                [ div_ [ class_ "content" ]
                  . renderDocStream
                  . reAnnotateS toStandardAnnotation
                  . layoutPretty defaultLayoutOptions
                  . annPretty
                  $ if model ^. #partiallyEvaluated
                    then ExprNode evaluationPlaceHolder
                    else capTree selectedNodeValueRenderDepth selectedNodeValue
                ]
         ])
        model

-- webPrint :: Miso.ToMisoString a => a -> View action
-- webPrint x = pre_ [] [ text $ Miso.ms x ]
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
