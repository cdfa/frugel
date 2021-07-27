{-# LANGUAGE DataKinds #-}
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

import Optics.Extra

import Prettyprinter.Render.String

import qualified Text.Megaparsec as Megaparsec

data EditResult = Success | Failure
    deriving ( Show, Eq )

data Direction = Leftward | Rightward | Upward | Downward
    deriving ( Show, Eq )

data GenericAction = Insert Char | Delete | Backspace | Move Direction

class ( Data p
      , Data (NodeOf p)
      , Decomposable p
      , Decomposable (NodeOf p)
      , CstrSiteNode p
      , CstrSiteNode (NodeOf p)
      , Parseable p
      ) => Editable p

deriving instance (Ord (Megaparsec.Token s), Ord e)
    => Ord (Megaparsec.ParseError s e)

updateModel :: forall p.
    (Editable p, DisplayProjection p)
    => GenericAction
    -> Model p
    -> (EditResult, Model p)
updateModel (Insert c) model = insert c model
updateModel Delete model = delete model
updateModel Backspace model = backspace model
updateModel (Move direction) model = moveCursor direction model

insert :: Editable p => Char -> Model p -> (EditResult, Model p)
insert c model = case editResult of
    Success -> edited & _2 % #cursorOffset +~ 1
    Failure -> edited
  where
    edited@(editResult, _)
        = attemptEdit (zipperAtCursor (Just . SeqZipper.insert (Left c))
                       $ view #cursorOffset model)
                      model

delete :: Editable p => Model p -> (EditResult, Model p)
delete model@Model{..} = case editResult of
    Success -> edited
    Failure -> edited & _2 % #errors .~ errors
  where
    edited@(editResult, _)
        = attemptEdit
            (zipperAtCursor
                 (suffixTail <=< guarded (is $ #suffix % ix 0 % _Left))
                 cursorOffset)
            model

backspace :: Editable p => Model p -> (EditResult, Model p)

-- when the bad default of "exiting" after a node when transformation failed is fixed
-- backspace model
--     = case attemptEdit
--         (zipperAtCursor prefixTail $ view #cursorOffset model)
--         model of
--         (Success, newModel) -> newModel & #cursorOffset -~ 1
--         (Failure, newModel) -> newModel & #errors .~ []
backspace model
    | view #cursorOffset model > 0
        = attemptEdit
            (zipperAtCursor
                 (suffixTail <=< guarded (is $ #suffix % ix 0 % _Left))
                 (view #cursorOffset model - 1))
        $ over #cursorOffset (subtract 1) model
backspace model = (Failure, model)

moveCursor
    :: DisplayProjection p => Direction -> Model p -> (EditResult, Model p)
moveCursor direction model@Model{..}
    = ( if cursorOffset == newOffset then Failure else Success
      , model & #cursorOffset .~ newOffset
      )
  where
    newOffset = updateOffset cursorOffset
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
        = renderString . layoutPretty defaultLayoutOptions $ renderDoc program

prettyPrint :: (Editable p, PrettyPrint p) => Model p -> Model p
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
        collectResults (Nothing, errors) cstrSiteBucket = case rights parses of
            [n] -> (Just n, newErrors) -- Only count success if it's the only one to be conservative in the presence of ambiguity
            _ -> (Nothing, newErrors)
          where
            newErrors
                = Set.union errors . Set.unions . fmap fromFoldable
                $ lefts parses
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
