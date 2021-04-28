module Frugel.Model ( module Frugel.Model, Model(Model) ) where

import           Frugel.Internal.Model
import           Frugel.Node
import           Frugel.Program

initialModel :: Model
initialModel
    = Model { program      = ProgramCstrSite defaultProgramMeta whereClauseTest
            , cursorOffset = 0
            , errors       = []
            }

fileName :: FilePath
fileName = "notepad"
