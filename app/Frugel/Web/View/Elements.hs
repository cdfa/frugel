module Frugel.Web.View.Elements where

import Frugel.Web.Action
import Frugel.Web.Event

import Miso

codeSpan :: [Attribute action] -> [View action] -> View action
codeSpan = span_ . (class_ "code-span" :)

inConstruction :: [Attribute action] -> [View action] -> View action
inConstruction = codeSpan . (class_ "in-construction node-padding" :)

complete :: [Attribute action] -> [View action] -> View action
complete = codeSpan . (class_ "has-background-white node-padding" :)

elided :: [Attribute action] -> [View action] -> View action
elided = codeSpan . (class_ "elided" :)

-- node :: [Attribute action] -> [View action] -> View action
-- node = codeSpan . (class_ "node" :)
caret :: [Attribute action] -> [View action] -> View action
caret = codeSpan . (class_ "caret" :)

codeRoot :: [Attribute Action] -> [View Action] -> View Action
codeRoot
    = button_ -- Using a button, because only (some) elements generate events
    . (++ [ keyDownHandler
          , id_ "code-root"
          , class_ "block no-button-style code-root"
          ])

line :: [Attribute action] -> [View action] -> View action
line = div_ . (class_ "line" :)
