module Model where

import           Node
import           Internal.Program ( Program(ProgramCstrSite) )
import           Internal.Meta    ( defaultProgramMeta )

data Model
    = Model { program :: Program, cursorOffset :: Integer, errors :: [String] }
    deriving ( Show, Eq )

initialModel :: Model
initialModel
    = Model { program
                  = ProgramCstrSite defaultProgramMeta Node.whereClauseTest
            , cursorOffset = 0
            , errors       = []
            }

fileName :: FilePath
fileName = "notepad"
