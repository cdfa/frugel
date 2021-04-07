module Frugel
    ( module Frugel
    , module Frugel.Model
    , module Frugel.PrettyPrinting
    , Layoutable(..)
    , Direction(..)
    , Action(..)
    , Model(Model, program, cursorOffset, errors)
    , parseErrorPretty
    , parseCstrSite
    ) where

import           Frugel.Action
import qualified Frugel.Internal.Model
import           Frugel.Layoutable
import           Frugel.Model
import           Frugel.Parsing
import           Frugel.PrettyPrinting

import           Miso                  hiding ( model, node, view )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel Load model = model <# do
    focus "code-root" >> pure NoOp
updateModel (Log msg) model = model <# do
    consoleLog (show msg) >> pure NoOp
updateModel (Insert c) model = noEff $ insert c model
updateModel (Move direction) model = noEff $ moveCursor direction model
updateModel PrettyPrint model = noEff $ prettyPrint model
