{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.Action where

import           Control.Monad.Except
import           Control.Zipper.Seq                      hiding ( insert )
import qualified Control.Zipper.Seq                      as SeqZipper

import qualified Data.Sequence                           as Seq
import qualified Data.Set                                as Set

import           Frugel.Decomposition
                 hiding ( ModificationStatus(Success) )
import qualified Frugel.Decomposition                    as Decomposition
import           Frugel.DisplayProjection
import           Frugel.Error
import           Frugel.Model
import           Frugel.Node
import           Frugel.Parsing
                 hiding ( node, program )
import           Frugel.PrettyPrinting
import           Frugel.Program

import           Miso
                 hiding ( focus, model, node, view )
import qualified Miso

import           Optics

import           Prettyprinter.Render.String
import           Prettyprinter.Render.Util.SimpleDocTree

import           Text.Megaparsec
                 hiding ( ParseError, errorOffset, parseErrorPretty )
import qualified Text.Megaparsec                         as Megaparsec

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
    | Backspace
    | Move Direction
    | PrettyPrint
    deriving ( Show, Eq )

deriving instance (Ord (Token s), Ord e) => Ord (Megaparsec.ParseError s e)

-- Updates model, optionally introduces side effects
updateModel :: Action -> Model -> Effect Action Model
updateModel NoOp model = noEff model
updateModel Load model = model <# do
    Miso.focus "code-root" >> pure NoOp
updateModel (Log msg) model = model <# do
    consoleLog (show msg) >> pure NoOp
updateModel (Insert c) model = noEff $ insert c model
updateModel Delete model = noEff $ delete model
updateModel Backspace model = noEff $ backspace model
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
-- and idem for backspace
delete :: Model -> Model
delete model
    = case attemptEdit
        (zipperAtCursor (suffixTail <=< guarded (is $ #suffix % ix 0 % _Left))
         $ view #cursorOffset model)
        model of
        (Success, newModel) -> newModel
        (Failure, newModel) -> newModel & #errors .~ []

backspace :: Model -> Model

-- when the bad default of "exiting" after a node when transformation failed is fixed
-- backspace model
--     = case attemptEdit
--         (zipperAtCursor prefixTail $ view #cursorOffset model)
--         model of
--         (Success, newModel) -> newModel & #cursorOffset -~ 1
--         (Failure, newModel) -> newModel & #errors .~ []
backspace model
    | view #cursorOffset model > 0
        = snd
        . attemptEdit
            (zipperAtCursor
                 (suffixTail <=< guarded (is $ #suffix % ix 0 % _Left))
                 (view #cursorOffset model - 1))
        $ over #cursorOffset (subtract 1) model
backspace model = model

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
        = renderString . layoutSmart defaultLayoutOptions . displayDoc
        $ view #program model

-- Leaves everything as construction site, because parsing nested construction sites is not implemented yet
prettyPrint :: Model -> Model
prettyPrint model = case attemptEdit (Right . prettyPrinted) model of
    (Success, newModel) -> newModel
    (Failure, newModel) -> newModel
        & #errors %~ cons (InternalError ParseFailedAfterPrettyPrint)
  where
    prettyPrinted
        = programCstrSite'
        . renderSimplyDecorated (fromList . map Left . toString) (const id)
        . treeForm
        . layoutSmart defaultLayoutOptions
        . annPretty

attemptEdit :: (Program -> Either InternalError Program)
    -> Model
    -> (EditResult, Model)
attemptEdit f model = case second reparse . f $ view #program model of
    Left editError -> (Failure, model & #errors .~ [ InternalError editError ])
    Right (newProgram, newErrors) ->
        (Success, model & #program .~ newProgram & #errors .~ newErrors)
  where
    reparse newProgram
        = bimap
            (flattenConstructionSites . fromMaybe newProgram)
            (map ParseError . toList)
        . foldr findSuccessfulParse (Nothing, mempty)
        . textVariations
        $ decompose newProgram
    findSuccessfulParse _ firstSuccessfulParse@(Just _, _)
        = firstSuccessfulParse
    findSuccessfulParse materials (Nothing, errors)
        = ( rightToMaybe parsed
          , maybe
                errors
                (Set.union errors
                 . fromFoldable
                 . fmap (normalizeErrorOffset materials))
            $ leftToMaybe parsed
          )
      where
        parsed = parseCstrSite fileName materials

normalizeErrorOffset :: CstrSite -> ParseError -> ParseError
normalizeErrorOffset (CstrSite materials) = errorOffset %~ \offset -> offset
    + sumOf
        (folded % _Right % to (pred . textLength))
        (Seq.take (offset - 1) materials)

-- construction sites with least nested construction sites should be at the end
textVariations :: CstrSite -> Seq CstrSite
textVariations
    = foldr processItem (Seq.singleton $ fromList []) . view _CstrSite
  where
    processItem item@(Left _) variations = cons item <$> variations
    processItem item@(Right node) variations
        = (if is _NodeCstrSite node then cons item <$> variations else mempty)
        <> (mappend <$> textVariations (decompose node) <*> variations)

type CstrSiteZipper = SeqZipper (Either Char Node)

zipperAtCursor :: (CstrSiteZipper -> Maybe CstrSiteZipper)
    -> Int
    -> Program
    -> Either InternalError Program
zipperAtCursor f
    = modifyNodeAt (\cstrSiteOffset materials -> maybeToRight
                        (CstrSiteActionFailed cstrSiteOffset materials)
                    $ traverseOf
                        _CstrSite
                        (rezip <.> f <=< unzipTo cstrSiteOffset)
                        materials)

step :: MonadState DecompositionState m => m ()
step = do
    textOffset <- use #textOffset
    -- c <- guse #cstrSiteOffset
    -- traceM ("step t: " <> show textOffset <> " c: " <> show c)
    when (textOffset /= -1) (#textOffset -= 1)
    when (textOffset > 0) (#cstrSiteOffset += 1)

modifyNodeAt :: forall m p.
    (MonadError InternalError m, Decomposable p, SetCstrSite p)
    => (Int -> CstrSite -> m CstrSite)
    -> Int
    -> p
    -> m p
modifyNodeAt f cursorOffset program
    = runModification >>= \(newProgram, decompositionState) -> if
        | view #textOffset decompositionState
            > 0 -> throwError $ DecompositionFailed cursorOffset
        | Todo <- view #modificationStatus decompositionState ->
            throwError $ ASTModificationNotPerformed cursorOffset
        | otherwise -> pure newProgram
  where
    runModification
        = mapNode program `runStateT` initialDecompositionState cursorOffset
    mapChar c = c <$ step -- trace (show c) step
    mapNode :: forall n.
        (Decomposable n, SetCstrSite n)
        => n
        -> DecompositionMonad m n
    -- mapNode n = do
        -- t <- guse #textOffset
        -- traceM ("pre " <> take 20 (show n) <> " " <> show t)
    mapNode n
        = ifM (guses #textOffset (< 0)) (pure n) -- node is located after cursor
        $ do
            #cstrSiteOffset
                += 1 -- At the moment, the construction site offset passed to `f` is immediately after a node where applying `f` failed. This is not a good default.
            withLocal #cstrSiteOffset 0 $ do
                newNode <- mapMComponents mapChar mapNode n
                -- traceM ("post " <> take 20 (show n))
                modificationStatus <- guse #modificationStatus
                ifM
                    (guses #textOffset (<= 0)
                     <&> (&& modificationStatus == Todo))
                    (catchError
                         (setCstrSite <$> transform n
                          ?? n
                          <* assign #modificationStatus Decomposition.Success)
                     $ const (pure n))
                    (pure
                     $ fromMaybe
                         newNode
                         (guard (modificationStatus == Decomposition.Success)
                          >> hasParser
                          >>= \parser -> getLast
                          . foldMap
                              (Last
                               . rightToMaybe
                               . runParser (parser <* eof) "")
                          . textVariations
                          $ decompose newNode))
    -- transform :: (Decomposable n) => n -> StateT DecompositionState m' CstrSite
    transform n = do
        cstrSiteOffset <- use #cstrSiteOffset
        lift $ case conservativelyDecompose cstrSiteOffset n of
            Just (cstrSiteOffset', cstrSite)
                | cstrSiteOffset == 0
                    || cstrSiteOffset == length (toList $ decompose n) ->
                    catchError (f cstrSiteOffset' cstrSite)
                    $ const (f cstrSiteOffset $ decompose n)
            _ -> f cstrSiteOffset $ decompose n