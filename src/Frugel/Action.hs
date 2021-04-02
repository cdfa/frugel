{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}

module Frugel.Action where

import           Frugel.Decomposition
import           Frugel.Meta
import           Frugel.Model
import           Frugel.Parsing       hiding ( program )
import           Frugel.Program

import           Optics

data Action = NoOp | Load | Log String | Insert Char | PrettyPrint
    deriving ( Show, Eq )

insert :: Char -> Model -> Model
insert c model = case reparsed of
    Left (inserted, newErrors) -> model
        & #program
        .~ maybe
            (view #program model)
            (ProgramCstrSite defaultProgramMeta)
            inserted
        & #errors .~ newErrors
    Right newProgram -> model & #program .~ newProgram & #errors .~ []
  where
    (materials, decomposeState)
        = runState (decomposed $ view #program model)
        $ initialDecompositionState
        $ view #cursorOffset model
    insert' = case decomposeState of
        DecompositionState _ textOffset
            | textOffset > 0 -> Left
                [ "Failed to decompose AST for cursor textOffset "
                  <> show (view #cursorOffset model)
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
