{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Frugel.Action where

import Control.Lens.Plated
import Control.ValidEnumerable
import Control.Zipper.Seq    hiding ( insert )
import qualified Control.Zipper.Seq as SeqZipper

import Data.Data
import Data.Data.Lens
import qualified Data.Set    as Set

import Frugel.CstrSite
import Frugel.Decomposition  hiding ( ModificationStatus(..) )
import Frugel.DisplayProjection
import Frugel.Error
import qualified Frugel.Internal.Model
import Frugel.Model
import Frugel.Parsing
import Frugel.PrettyPrinting hiding ( prettyPrint )
import qualified Frugel.PrettyPrinting as PrettyPrinting

import Miso                  hiding ( focus, model, node, set, view )
import qualified Miso        hiding ( set )

import Optics.Extra

import Prettyprinter.Render.String

import Test.QuickCheck.Gen

import qualified Text.Megaparsec as Megaparsec

data EditResult = Success | Failure
    deriving ( Show, Eq )

data Direction = Leftward | Rightward | Upward | Downward
    deriving ( Show, Eq )

data Action p
    = Load
    | GenerateRandom
    | NewModel (Model p)
    | Log String
    | Insert Char
    | Delete
    | Backspace
    | Move Direction
    | PrettyPrint

class ( Data p
      , Data (NodeOf p)
      , Decomposable p
      , Decomposable (NodeOf p)
      , CstrSiteNode p
      , CstrSiteNode (NodeOf p)
      , Parseable p
      , Ord (ParseErrorOf p)
      , PrettyPrint p
      ) => Editable p

deriving instance (Ord (Megaparsec.Token s), Ord e)
    => Ord (Megaparsec.ParseError s e)

-- Updates model, optionally introduces side effects
updateModel :: (Editable p, DisplayProjection p, ValidEnumerable p)
    => Action p
    -> Model p
    -> Effect (Action p) (Model p)
updateModel Load model
    = fromTransition (scheduleIO_ $ Miso.focus "code-root") model
updateModel GenerateRandom model = model <# do
    NewModel . flip (set #program) model
        <$> (liftIO . generate $ uniformValid 500)
updateModel (NewModel model) _ = noEff model
updateModel (Log msg) model
    = fromTransition (scheduleIO_ . consoleLog $ show msg) model
updateModel (Insert c) model = noEff $ insert c model
updateModel Delete model = noEff $ delete model
updateModel Backspace model = noEff $ backspace model
updateModel (Move direction) model = noEff $ moveCursor direction model
updateModel PrettyPrint model = noEff $ prettyPrint model

insert :: (Editable p) => Char -> Model p -> Model p
insert c model
    = case attemptEdit (zipperAtCursor (Just . SeqZipper.insert (Left c))
                        $ view #cursorOffset model)
                       model of
        (Success, newModel) -> newModel & #cursorOffset +~ 1
        (Failure, newModel) -> newModel

-- This only works as long as there is always characters (or nothing) after nodes in construction sites
-- and idem for backspace
delete :: (Editable p) => Model p -> Model p
delete model@Model{..}
    = case attemptEdit
        (zipperAtCursor (suffixTail <=< guarded (is $ #suffix % ix 0 % _Left))
                        cursorOffset)
        model of
        (Success, newModel) -> newModel
        (Failure, newModel) -> newModel & #errors .~ errors

backspace :: (Editable p) => Model p -> Model p

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

moveCursor :: DisplayProjection p => Direction -> Model p -> Model p
moveCursor direction model = model & #cursorOffset %~ updateOffset
  where
    updateOffset = case direction of
        Leftward -> max 0 . subtract 1
        Rightward -> min (length programText) . (+ 1)
         -- extra subtract 1 for the \n
        Upward -> case leadingLines of
            ((_ :> previousLine) :> leadingChars) -> max 0
                . subtract (length leadingChars -- rest of the current line
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
    -- uses layoutPretty for consistency with view
    programText
        = renderString . layoutPretty defaultLayoutOptions . renderDoc
        $ view #program model

prettyPrint :: forall p. Editable p => Model p -> Model p
prettyPrint model@Model{..}
    = model
    & #program .~ newProgram
    & #errors .~ map ParseError newErrors
    & #cursorOffset .~ min cursorOffset (textLength newProgram)
  where
    (newProgram, newErrors) = PrettyPrinting.prettyPrint program

attemptEdit :: forall p.
    Editable p
    => (p -> Either (InternalError p) p)
    -> Model p
    -> (EditResult, Model p)
attemptEdit
    f
    model = case second (reparse programParser) . f $ view #program model of
    Left editError -> (Failure, model & #errors .~ [ InternalError editError ])
    Right (newProgram, newErrors) ->
        ( Success
        , model
          & #program .~ flattenConstructionSites newProgram
          & #errors .~ map ParseError (toList newErrors)
        )
  where
    reparse :: forall n.
        (NodeOf p ~ NodeOf n, Data n, Decomposable n)
        => (ParserOf p) n
        -> n
        -> (n, Set (ParseErrorOf p))
    reparse parser node
        = uncurry (reparseNestedCstrSites @p reparse)
        . first (fromMaybe (decompose node, node))
        . findSuccessfulParse
        . groupSortOn cstrSiteCount
        . inliningVariations
        $ decompose node
      where
        findSuccessfulParse = foldl' collectResults (Nothing, mempty)
        collectResults firstSuccessfulParse@(Just _, _) _
            = firstSuccessfulParse
        collectResults (Nothing, errors) cstrSiteBucket
            = ( head <.> nonEmpty $ rights parses -- It would be possible to do some ambiguity checking by keeping track of ambiguously resolved construction sites across construction sites buckets, but as long as there is no way of parsing a nested construction site without considering the parent, these cases are so rare the checking is not worth the added complexity
              , Set.union errors . Set.unions . fmap fromFoldable
                $ lefts parses
              )
          where
            parses
                = map (\cstrSite ->
                       second (cstrSite, ) $ runParser @p parser cstrSite)
                      cstrSiteBucket

flattenConstructionSites :: forall n.
    ( Data n
    , Typeable (NodeOf n)
    , Data (NodeOf n)
    , CstrSiteNode (NodeOf n)
    , NodeOf n ~ NodeOf (NodeOf n)
    )
    => n
    -> n
flattenConstructionSites
    = transformOnOf (template @n @(ACstrSite (NodeOf n))) uniplate
    $ foldMapOf (_CstrSite % folded)
                (\item -> fromMaybe (one item) (item ^? _Right % _NodeCstrSite))

inliningVariations :: (n ~ NodeOf n, CstrSiteNode n, Decomposable n)
    => ACstrSite n
    -> [ACstrSite n]
inliningVariations = foldr addItem [ fromList [] ] . view _CstrSite
  where
    addItem item@(Left _) variations = cons item <$> variations
    -- It would be more efficient to have the node's inlining variations also saved in the variation where the node's construction site is not inlined
    -- (it's now recomputed in `reparseNestedConstructionSites`)
    addItem item@(Right node) variations
        = (if is _NodeCstrSite node then cons item <$> variations else mempty)
        <> (mappend <$> inliningVariations (decompose node) <*> variations)

type CstrSiteZipper n = SeqZipper (Either Char (NodeOf n))

zipperAtCursor :: (Decomposable p, CstrSiteNode p)
    => (CstrSiteZipper p -> Maybe (CstrSiteZipper p))
    -> Int
    -> p
    -> Either (InternalError p) p
zipperAtCursor f
    = modifyNodeAt (\cstrSiteOffset materials ->
                    maybeToRight (CstrSiteActionFailed cstrSiteOffset materials)
                    $ traverseOf _CstrSite
                                 (rezip <.> f <=< unzipTo cstrSiteOffset)
                                 materials)
