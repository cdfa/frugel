module Frugel.Model ( module Frugel.Model, Model(Model) ) where

import           Frugel.Internal.Model
import           Frugel.Meta
import           Frugel.Program

initialModel :: Model
initialModel
    = Model { program      = ProgramCstrSite defaultProgramMeta $ fromList []
            , cursorOffset = 0
            , errors       = []
            }

fileName :: FilePath
fileName = "notepad"
