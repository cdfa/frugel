{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module Frugel.Model
    , module Frugel.PrettyPrinting
    , Action(..)
    , parseErrorPretty
    , parseCstrSite
    ) where

import           Frugel.Action
import           Frugel.Model
import           Frugel.Parsing
import           Frugel.PrettyPrinting

import           Miso                  hiding ( node )

import           Text.Megaparsec

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel Load m = m <# do
    focus "code-root" >> pure NoOp
updateModel (Insert c) m = noEff $ insert c m
updateModel (Log msg) m = m <# do
    consoleLog (show msg) >> pure NoOp
