module Frugel.Model ( module Frugel.Model, Model(Model) ) where

import Frugel.Internal.Model

initialModel :: p -> Model p
initialModel initialProgram
    = Model { program = initialProgram, cursorOffset = 0, errors = [] }
