{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE TupleSections #-}

module Frugel.Action where

import           Control.Zipper.Seq          hiding ( delete, insert )
import qualified Control.Zipper.Seq          as SeqZipper

import           Data.Composition

import           Frugel.Decomposition
import           Frugel.Layoutable
import           Frugel.Meta
import           Frugel.Model
import           Frugel.Node
import           Frugel.Parsing              hiding ( program )
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Miso                        hiding ( focus, model, node, view )
import qualified Miso

import           Optics

import           Prettyprinter.Render.String

data Direction = Leftward | Rightward | Upward | Downward
    deriving ( Show, Eq )

data Action
    = NoOp | Load | Log String | Insert Char | Move Direction | PrettyPrint
    deriving ( Show, Eq )

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel Load model = model <# do
    Miso.focus "code-root" >> pure NoOp
updateModel (Log msg) model = model <# do
    consoleLog (show msg) >> pure NoOp
updateModel (Insert c) model = noEff $ insert c model
updateModel (Move direction) model = noEff $ moveCursor direction model
updateModel PrettyPrint model = noEff $ prettyPrint model

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
        = zipperAtCursor (SeqZipper.insert $ Left c) (view #cursorOffset model)
        $ view #program model
    reparsed = do
        inserted <- first (Nothing, ) insert'
        first ((Just inserted, ) . map parseErrorPretty . toList)
            $ parseCstrSite fileName inserted

moveCursor :: Direction -> Model -> Model
moveCursor direction model = model & #cursorOffset %~ updateOffset
  where
    updateOffset = case direction of
        Leftward -> max 0 . subtract 1
        Rightward -> min (length programText) . (+ 1)
        Upward -> maybe
            (const currentOffset)
            ((max 0 . subtract 1) .: subtract)
            previousLineLength -- extra subtract 1 for the \n
        Downward -> if length trailingLines <= 1
            then const currentOffset
            else min (length programText) . (currentLineLength + 1 +) -- +1 for the \n
    previousLineLength
        = length . head
        <$> (nonEmpty . tail =<< nonEmpty (reverse leadingLines))
    currentLineLength
        = length
            (concat (last <$> nonEmpty leadingLines)
             ++ concat (head <$> nonEmpty trailingLines))
    (leadingLines, trailingLines)
        = splitAt currentOffset programText & both %~ splitOn '\n'
    currentOffset = view #cursorOffset model
    programText
        = renderString . layoutSmart defaultLayoutOptions . layoutDoc
        $ view #program model

-- For now, pretty printing only works on complete programs, because correct pretty printing of complete nodes in construction sites is difficult
-- It would require making a parser that skips all the construction materials and then putting the new whitespace back in the old nodes
prettyPrint :: Model -> Model
prettyPrint model = case view #program model of
    p@Program{} -> case prettyPrinted p of
        Left errors -> model
            & #errors
            .~ ("Internal error: failed to reparse a pretty-printed program"
                : map show (toList errors))
        Right newProgram -> model & #program .~ newProgram
      where
        prettyPrinted
            = parseCstrSite fileName
            . fromList
            . map Left
            . renderString
            . layoutSmart defaultLayoutOptions
            . annPretty
    _ -> model & #errors %~ ("Can't pretty print a construction site" :)

zipperAtCursor :: (SeqZipper (Either Char Node) -> SeqZipper (Either Char Node))
    -> Int
    -> Program
    -> Either [Doc Annotation] CstrMaterials
zipperAtCursor f cursorOffset program = case decompose cursorOffset program of
    Nothing -> Left
        [ "Failed to decompose AST for cursor textOffset " <> show cursorOffset
        ]
    Just (cstrMaterialOffset, materials) -> maybeToRight
        [ "Failed to modify the construction site "
          <> layoutDoc materials
          <> " at index "
          <> show cstrMaterialOffset
        ]
        $ traverseOf
            _CstrMaterials
            (rezip <.> f <.> unzipTo cstrMaterialOffset)
            materials
