{-# LANGUAGE FlexibleContexts #-}

module Frugel
    ( module Frugel
    , module Frugel.Model
    , module Frugel.PrettyPrinting
    , Layoutable(..)
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
import           Frugel.Program

import           Miso
                 hiding ( model, node, view )

import           Optics

import           Prettyprinter.Render.Util.StackMachine

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel Load model = model <# do
    focus "code-root" >> pure NoOp
updateModel (Log msg) model = model <# do
    consoleLog (show msg) >> pure NoOp
updateModel (Insert c) model = noEff $ insert c model
-- For now, pretty printing only works on complete programs, because correct pretty printing of complete nodes in construction sites is difficult
-- It would require making a parser that skips all the construction materials and then putting the new whitespace back in the old nodes
updateModel PrettyPrint model = noEff $ case view #program model of
    p@Program{} -> case prettyPrinted p of
        Left errors -> model
            & #errors
            .~ ("Internal error: failed to reparse a pretty-printed program"
                : map show (toList errors))
        Right newProgram -> model & #program .~ newProgram
      where
        prettyPrinted
            = parseCstrSite fileName
            . fromList
            . map Left
            . toString
            . renderSimplyDecorated id (const "") (const "")
            . layoutSmart defaultLayoutOptions
            . annPretty
    _ -> model & #errors %~ ("Can't pretty print a construction site" :)
