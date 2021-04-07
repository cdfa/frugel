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
        & if isJust inserted then #cursorOffset +~ 1 else id
    Right newProgram ->
        model & #program .~ newProgram & #errors .~ [] & #cursorOffset +~ 1
  where
    insert'
        = case decompose (view #cursorOffset model) $ view #program model of
            Nothing -> Left
                [ "Failed to decompose AST for cursor textOffset "
                  <> show (view #cursorOffset model)
                ]
            Just (cstrMaterialOffset, materials) -> maybeToRight
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
