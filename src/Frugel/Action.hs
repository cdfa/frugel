{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.Action where

import           Control.Zipper.Seq          hiding ( delete, insert )
import qualified Control.Zipper.Seq          as SeqZipper

import qualified Data.Sequence               as Seq
import qualified Data.Set                    as Set

import           Frugel.Decomposition        hiding ( ModificationStatus(..) )
import           Frugel.Layoutable
import           Frugel.Meta
import           Frugel.Model
import           Frugel.Node
import           Frugel.Parsing              hiding ( node, program )
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Miso                        hiding ( focus, model, node, view )
import qualified Miso

import           Optics

import           Prettyprinter.Render.String

import           Text.Megaparsec             hiding ( parseErrorPretty )

data EditResult = Success | Failure
    deriving ( Show, Eq )

data Direction = Leftward | Rightward | Upward | Downward
    deriving ( Show, Eq )

data Action
    = NoOp
    | Load
    | Log String
    | Insert Char
    | Delete
    | Move Direction
    | PrettyPrint
    deriving ( Show, Eq )

deriving instance (Ord (Token s), Ord e) => Ord (ParseError s e)

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel Load model = model <# do
    Miso.focus "code-root" >> pure NoOp
updateModel (Log msg) model = model <# do
    consoleLog (show msg) >> pure NoOp
updateModel (Insert c) model = noEff $ insert c model
updateModel Delete model = noEff $ delete model
updateModel (Move direction) model = noEff $ moveCursor direction model
updateModel PrettyPrint model = noEff $ prettyPrint model

insert :: Char -> Model -> Model
insert c model
    = case attemptEdit
        (zipperAtCursor (Just . SeqZipper.insert (Left c))
         $ view #cursorOffset model)
        model of
        (Success, newModel) -> newModel & #cursorOffset +~ 1
        (Failure, newModel) -> newModel

-- This only works as long as there is always characters (or nothing) after nodes in construction sites
delete :: Model -> Model
delete model
    = snd
    $ attemptEdit
        (zipperAtCursor SeqZipper.delete $ view #cursorOffset model)
        model

moveCursor :: Direction -> Model -> Model
moveCursor direction model = model & #cursorOffset %~ updateOffset
  where
    updateOffset = case direction of
        Leftward -> max 0 . subtract 1
        Rightward -> min (length programText) . (+ 1)
         -- extra subtract 1 for the \n
        Upward -> case leadingLines of
            ((_ :> previousLine) :> leadingChars) -> max 0
                . subtract
                    (length leadingChars -- rest of the current line
                     + 1 -- \n
                     + max 0 (length previousLine - length leadingChars)) -- end of or same column on the previous line
            _ -> const currentOffset
        Downward -> case (leadingLines, trailingLines) of
            (_ :> leadingChars, trailingChars : (nextLine : _)) -> min
                (length programText)
                . (length trailingChars -- rest of the current line
                   + 1 -- \n
                   + min (length nextLine) (length leadingChars) +) -- end of or same column on the next line
            _ -> const currentOffset
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
    Program{} -> case attemptEdit (Right . prettyPrinted) model of
        (Success, newModel) -> newModel
        (Failure, newModel) -> newModel
            & #errors
            %~ cons
                "Internal error: failed to reparse a pretty-printed program"
      where
        prettyPrinted
            = ProgramCstrSite defaultProgramMeta
            . fromList
            . map Left
            . renderString
            . layoutSmart defaultLayoutOptions
            . annPretty
    _ -> model & #errors %~ ("Can't pretty print a construction site" :)

attemptEdit :: (Program -> Either [Doc Annotation] Program)
    -> Model
    -> (EditResult, Model)
attemptEdit f model = case reparsed of
    Left editErrors -> (Failure, model & #errors .~ editErrors)
    Right (newProgram, newErrors) ->
        (Success, model & #program .~ newProgram & #errors .~ newErrors)
  where
    reparsed
        = second (\newProgram ->
                  bimap (fromMaybe newProgram) (map parseErrorPretty . toList)
                  . foldr findSuccessfulParse (Nothing, mempty)
                  . textVariations
                  $ decomposed newProgram) . f $ view #program model
    findSuccessfulParse _ firstSuccessfulParse@(Just _, _)
        = firstSuccessfulParse
    findSuccessfulParse materials (Nothing, errors)
        = ( rightToMaybe parsed
          , maybe errors (Set.union errors . fromFoldable) $ leftToMaybe parsed
          )
      where
        parsed = parseCstrSite fileName materials

-- construction sites with least nested construction sites should be at the end
textVariations :: CstrMaterials -> Seq CstrMaterials
textVariations
    = foldr processItem (Seq.singleton $ fromList []) . view _CstrMaterials
  where
    processItem item@(Left _) variations = cons item <$> variations
    processItem item@(Right node) variations
        | Just nodeMaterials <- preview _CstrSiteNode node
            = fmap (cons item) variations
            <> (mappend <$> textVariations nodeMaterials <*> variations)
    processItem (Right node) variations
        = mappend <$> textVariations (decomposed node) <*> variations

zipperAtCursor :: (CstrMaterialsZipper -> Maybe CstrMaterialsZipper)
    -> Int
    -> Program
    -> Either [Doc Annotation] Program
zipperAtCursor f
    = modifyNodeAt
        (\cstrSiteOffset materials -> maybeToRight
             [ "Internal error: Failed to modify the construction site "
               <> layoutDoc materials
               <> " at index "
               <> show cstrSiteOffset
             ]
         $ traverseOf
             _CstrMaterials
             (rezip <.> f <=< unzipTo cstrSiteOffset)
             materials)
