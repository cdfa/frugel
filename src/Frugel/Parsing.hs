{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frugel.Parsing where

import qualified Control.Lens as Lens
import Control.Monad.Writer

import Data.Data
import Data.Data.Lens
import qualified Data.Sequence as Seq
import Data.Set.Optics

import Frugel.CstrSite
import Frugel.Decomposition

import Optics.Extra

class Ord (ParseErrorOf p) => Parseable p where
    type ParserOf p :: * -> *
    type ParseErrorOf p :: *
    programParser :: (ParserOf p) p
    anyNodeParser :: (ParserOf p) (NodeOf p)
    runParser :: (ParserOf p) n
        -> ACstrSite (NodeOf p)
        -> Either (NonEmpty (ParseErrorOf p)) n
    errorOffset :: Lens' (ParseErrorOf p) Int

fixErrorOffset :: forall p.
    (Decomposable (NodeOf p), NodeOf p ~ NodeOf (NodeOf p), Parseable p)
    => ACstrSite (NodeOf p)
    -> ParseErrorOf p
    -> ParseErrorOf p
fixErrorOffset (CstrSite materials) = errorOffset @p %~ \offset -> offset
    + sumOf (folded % _Right % to (pred . textLength))
            (Seq.take (offset - 1) materials)

reparseNestedCstrSites :: forall p n.
    ( Data n
    , Typeable (NodeOf n)
    , Decomposable n
    , Decomposable (NodeOf n)
    , NodeOf p ~ NodeOf n
    , Parseable p
    )
    => (ParserOf p (NodeOf n) -> NodeOf n -> (NodeOf n, Set (ParseErrorOf p)))
    -> (ACstrSite (NodeOf p), n)
    -> Set (ParseErrorOf p)
    -> (n, Set (ParseErrorOf p))
reparseNestedCstrSites reparse (cstrSite, newNode) errors
    = runWriter
    $ tell errors
    >> Lens.itraverseOf
        (Lens.indexing $ template @n @(NodeOf n))
        (\i ->
         writer . second (increaseErrorOffsets i) . reparse (anyNodeParser @p))
        newNode
  where
    increaseErrorOffsets i
        = setmapped % errorOffset @p
        +~ fst
            (Seq.filter (isRight . snd) (leadingCumulativeTextLengths cstrSite)
             `Seq.index` i)
    -- not exactly a prefix sum; first element of pair is text length of construction materials before the item in the right element
    leadingCumulativeTextLengths (CstrSite materials)
        = snd
        $ mapAccumL
            (\l item -> (l + either (const 1) textLength item, (l, item)))
            0
            materials
