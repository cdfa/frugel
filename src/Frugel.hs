{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module Model
    , Action(..)
    , prettyProgram
    , prettyCstrMaterials
    , parseErrorPretty
    , parseCstrSite
    ) where

import           Miso             hiding ( node )
import           Text.Megaparsec
import           Node
import           Internal.Program ( prettyProgram )
import           Parsing
import           Model
import           Action

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp m = noEff m
updateModel (Insert c) m = noEff $ insert c m
updateModel (Log msg) m = m <# do
    consoleLog (show msg) >> pure NoOp
