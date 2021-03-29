module Model where

import           Node
import           Internal.Program    ( Program(ProgramCstrSite) )
import           Text.Megaparsec.Pos
import           Internal.Meta       ( defaultProgramMeta )

data Model = Model { program :: Program, cursorPos :: SourcePos }
    deriving ( Show, Eq )

initialModel :: Model
initialModel
    = Model { program
                  = ProgramCstrSite defaultProgramMeta Node.whereClauseTest
            , cursorPos = initialPos fileName
            }

fileName :: FilePath
fileName = "notepad"
