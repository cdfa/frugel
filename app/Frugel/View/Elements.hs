module Frugel.View.Elements where

import Frugel
import Frugel.Event

import Miso
import qualified Miso.String

noButtonStyle :: Attribute action
noButtonStyle
    = style_
    $ "background" =: "none"
    <> "color" =: "inherit"
    <> "border" =: "none"
    <> "padding" =: "0"
    <> "font" =: "inherit"
    <> "cursor" =: "auto"
    <> "outline" =: "inherit"
    <> "user-select" =: "text"
    <> "text-align" =: "left"

codeStyle :: Attribute action
codeStyle
    = style_
    $ "white-space" =: "pre" <> "font-family" =: "\"Courier New\", monospace"

spanStyle :: Attribute action
spanStyle = style_ $ "display" =: "inline-block"

span :: [Attribute action] -> [View action] -> View action
span = span_ . (spanStyle :)

paddingStyle :: Attribute action
paddingStyle
    = style_ $ "padding" =: Miso.String.ms (unwords [ "4px", "0", "4px", "0" ])

inConstructionStyles :: [Attribute action]
inConstructionStyles
    = [ style_
        $ "background-color" =: "hsl(48, 100%, 85%)" <> "min-width" =: "0.6em"
      , paddingStyle
      ]

inConstruction :: [Attribute action] -> [View action] -> View action
inConstruction = span . (++ inConstructionStyles)

completeStyles :: [Attribute action]
completeStyles = [ class_ "has-background-white", paddingStyle ]

complete :: [Attribute action] -> [View action] -> View action
complete = span . (++ completeStyles)

node :: [Attribute action] -> [View action] -> View action
node = span . (class_ "node" :)

caretStyle :: Attribute action
caretStyle
    = style_
    $ "padding" =: "0 1px"
    <> "margin" =: "0 -1px -0.2em -1px"
    <> "height" =: "1.2em"
    <> "background-color" =: "green"

caret :: [Attribute action] -> [View action] -> View action
caret = span . (caretStyle :)

codeRoot :: [Attribute (Action p)] -> [View (Action p)] -> View (Action p)
codeRoot
    = button_ -- Using a button, because only (some) elements generate events
    . (++ [ keyDownHandler
          , noButtonStyle
          , id_ "code-root"
          , style_ $ "margin-left" =: "10px" <> "margin-top" =: "10px"
          ])

lineStyle :: Attribute action
lineStyle = style_ $ "min-height" =: "2em"

line :: [Attribute action] -> [View action] -> View action
line = div_ . (lineStyle :)
