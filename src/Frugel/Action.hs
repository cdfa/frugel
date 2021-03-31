{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}

module Frugel.Action where

import           Frugel.Decomposition
import           Frugel.Meta
import           Frugel.Model         as Model
import           Frugel.Parsing
import           Frugel.Program

import           Optics

import           Text.Megaparsec

data Action = NoOp | Load | Insert Char | Log String
    deriving ( Show, Eq )

insert :: Char -> Model -> Model
insert c model = case reparsed of
    Left (inserted, newErrors) ->
        model { Model.program = maybe
                    (Model.program model)
                    (ProgramCstrSite defaultProgramMeta)
                    inserted
              , errors        = newErrors
              }
    Right newProgram -> model { Model.program = newProgram, errors = [] }
  where
    (materials, decomposeState)
        = runState (decomposed $ Model.program model)
        $ initialDecompositionState
        $ cursorOffset model
    insert' = case decomposeState of
        DecompositionState _ textOffset
            | textOffset > 0 -> Left
                [ "Failed to decompose AST for cursor textOffset "
                  <> show (cursorOffset model)
                ]
        DecompositionState cstrMaterialOffset _ -> maybeToRight
            [ "Failed to insert '"
              <> show c
              <> "' into construction site at index "
              <> show cstrMaterialOffset
            ]
            $ insertAt cstrMaterialOffset (Left c) materials
    reparsed = do
        inserted <- first (Nothing, ) insert'
        first ((Just inserted, ) . map parseErrorPretty . toList)
            $ parseCstrSite fileName inserted
