module Frugel.Model where

import           Frugel.Meta
import           Frugel.Node
import           Frugel.Program

data Model
    = Model { program :: Program, cursorOffset :: Integer, errors :: [String] }
    deriving ( Show, Eq )

initialModel :: Model
initialModel
    = Model { program      = ProgramCstrSite defaultProgramMeta parensTest
            , cursorOffset = 0
            , errors       = []
            }

fileName :: FilePath
fileName = "notepad"
