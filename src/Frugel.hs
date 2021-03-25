{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module Model
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

-- Sum type for application events
-- data Action = AddOne | SubtractOne | NoOp | SayHelloWorld
data Action = NoOp
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model

-- updateModel AddOne m = noEff (m + 1)
-- updateModel SubtractOne m = noEff (m - 1)
updateModel NoOp = noEff
-- updateModel SayHelloWorld m =
--     m <# do liftIO (putStrLn "Hello World") >> pure NoOp
