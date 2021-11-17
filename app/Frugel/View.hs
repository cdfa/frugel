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

import Scout           hiding ( Evaluated )
import Scout.Action
import qualified Scout.Internal.Model
import Scout.Model

-- import Text.Show.Pretty ( ppShow )
-- Constructs a virtual DOM from a model
viewApp :: Model -> View Action
viewApp model
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
                    , onChange (ChangeFuelLimit
                                . fromMaybe fuelLimit
                                . readMaybe
                                . Miso.fromMisoString)
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
           . layoutPretty defaultLayoutOptions
           . renderDoc)
          errors

evaluatedView :: Model -> View Action
evaluatedView model@Model{..}
    = div_ [ class_ "card" ]
    $ [ div_ [ class_ "card-header" ]
             [ p_ [ class_ "card-header-title" ] [ case evaluationStatus of
                 Evaluated -> "Result"
                 PartiallyEvaluated -> "Evaluating..."
                 Aborted msg ->
                     text $ "Evaluation aborted due to " <> Miso.ms msg ] ]
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
        #selectedNodeEvaluation
        (\selectedNodeEvaluation ->
         [ div_ [ class_ "card-header" ]
                [ p_ [ class_ "card-header-title" ]
                     [ "Focused node evaluation" ]
                , button_ [ class_ "card-header-icon"
                          , onClick (ChangeFocusedNodeEvaluationIndex Decrement)
                          ]
                          [ span_ [ class_ "icon" ] [ text "ᐊ" ] ]
                , span_
                      [ class_ "card-header-vertical-padding" ]
                      [ let focusedNodeEvaluationsCount
                                = Seq.length
                                $ view #focusedNodeEvaluations evaluationOutput
                        in text
                           $ show (min focusedNodeEvaluationsCount
                                       (selectedNodeEvaluationIndex + 1))
                           <> " of "
                           <> show focusedNodeEvaluationsCount
                      ]
                , button_ [ class_ "card-header-icon"
                          , onClick (ChangeFocusedNodeEvaluationIndex Increment)
                          ]
                          [ span_ [ class_ "icon" ] [ text "ᐅ" ] ]
                ]
         , div_ [ class_ "card-content" ]
                [ selectedNodeEvaluationView selectedNodeEvaluation model ]
         ])
        model

selectedNodeEvaluationView :: FocusedNodeEvaluation -> Model -> View Action
selectedNodeEvaluationView selectedNodeEvaluation model@Model{..}
    = div_ [ class_ "card" ]
    $ conditionalViews
        (variablesInView model)
        [ div_ [ class_ "card-header" ]
               [ p_ [ class_ "card-header-title" ]
                    [ "Variables in scope at the cursor" ]
               , div_ [ class_ "card-header-vertical-padding" ]
                      [ "depth: ", renderDepthInput Value model ]
               ]
        , div_ [ class_ "card-content" ]
               [ div_ [ class_ "content" ]
                 . map (div_ []
                        . renderPretty
                        . capTree contextRenderDepth
                        . DeclNode
                        . uncurry decl')
                 . toList
                 $ view #variables selectedNodeEvaluation
               ]
        ]
    ++ conditionalViews
        (has (#definitions % folded) selectedNodeEvaluation)
        (div_
             [ class_ "card-header" ]
             [ p_ [ class_ "card-header-title" ]
                  [ "Definitions in scope at the cursor" ]
             , div_ [ class_ "card-header-vertical-padding" ]
                    [ "depth: ", renderDepthInput Value model ]
             , button_
                   [ class_ "card-header-icon", onClick ToggleDefinitionsView ]
                   [ span_
                         [ class_ "icon" ]
                         [ text
                           $ if definitionsViewCollapsed then "▽" else "△"
                         ]
                   ]
             ]
         : conditionalViews
             (not (view #definitionsViewCollapsed model))
             [ div_ [ class_ "card-content" ]
                    [ div_ [ class_ "content" ]
                      . map (div_ []
                             . renderPretty
                             . capTree contextRenderDepth
                             . DeclNode
                             . uncurry decl')
                      . toList
                      $ view #definitions selectedNodeEvaluation
                    ]
             ])
    ++ [ div_ [ class_ "card-header" ]
              [ p_ [ class_ "card-header-title" ] [ "Focused node value" ]
              , div_ [ class_ "card-header-vertical-padding" ]
                     [ "depth: ", renderDepthInput Value model ]
              ]
       , div_ [ class_ "card-content" ]
              [ div_ [ class_ "content" ]
                . renderPretty
                . capTree selectedNodeValueRenderDepth
                $ view #value selectedNodeEvaluation
              ]
       ]

renderDepthInput :: RenderDepthField -> Model -> View Action
renderDepthInput field model
    = input_ [ type_ "number"
             , value_ . Miso.ms $ show @String renderDepth
             , onChange (ChangeFieldRenderDepth field
                         . fromMaybe renderDepth
                         . readMaybe
                         . Miso.fromMisoString)
             ]
  where
    renderDepth = view (renderDepthFieldLens field) model

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
