{-# LANGUAGE FlexibleContexts #-}

module Action where

import           Prelude             hiding ( one )
import           Model
import           Internal.Meta       ( defaultProgramMeta )
import           Internal.Program    ( Program(ProgramCstrSite) )
import           Parsing
import           Decomposition

import           Text.Megaparsec.Pos
import           Optics

insert :: Model -> Char -> Model
insert model c
    = model { Model.program = fromRight
                  (ProgramCstrSite defaultProgramMeta inserted)
                  $ parseCstrSite fileName inserted
            }
  where
    (materials, decomposeState)
        = runState (decomposed $ Model.program model)
        $ initialDecompositionState cursorOffset
    inserted = case decomposeState of
        DecompositionState _ textOffset
            | textOffset /= -1 -> error
                ("Failed to decompose AST for cursor textOffset "
                 <> show cursorOffset)
        DecompositionState cstrMaterialOffset _ -> fromMaybe
            (error
                 ("Failed to insert '"
                  <> show c
                  <> "' into construction site at index "
                  <> show cstrMaterialOffset))
            $ insertAt cstrMaterialOffset (Left c) materials
    cursorOffset = offSetBySourcePos $ cursorPos model

offSetBySourcePos :: SourcePos -> Integer
offSetBySourcePos = error "not implemented"
