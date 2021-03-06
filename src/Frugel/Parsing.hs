{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frugel.Parsing where

import qualified Control.Lens as Lens
import Control.Monad.Writer.Strict

import Data.Data
import Data.Data.Lens
import qualified Data.Sequence as Seq
import Data.Set.Optics

import Frugel.CstrSite
import Frugel.Decomposition

import Optics.Extra.Frugel

class Ord (ParseErrorOf p) => Parseable p where
    type ParserOf p :: Type -> Type
    type ParseErrorOf p :: Type
    programParser :: (ParserOf p) p
    anyNodeParser :: (ParserOf p) (NodeOf p)
    runParser :: (ParserOf p) n
        -> ACstrSite (NodeOf p)
        -> Either (NonEmpty (ParseErrorOf p)) ([ParseErrorOf p], n)
    errorOffset :: Lens' (ParseErrorOf p) Int
    consumedEmptyCstrSiteCount :: ([ParseErrorOf p], n) -> Int

fixErrorOffset :: forall p.
    (Decomposable (NodeOf p), NodeOf p ~ NodeOf (NodeOf p), Parseable p)
    => ACstrSite (NodeOf p)
    -> ParseErrorOf p
    -> ParseErrorOf p
fixErrorOffset (CstrSite components) = errorOffset @p %~ \offset -> offset
    + sumOf (folded % _Right % to (pred . textLength))
            (Seq.take (offset - 1) components)

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
    -> (n, Set (ParseErrorOf p))
reparseNestedCstrSites reparse (cstrSite, newNode)
    = runWriter
    $ Lens.itraverseOf
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
    -- not exactly a prefix sum; first element of pair is text length of construction components before the item in the right element
    leadingCumulativeTextLengths (CstrSite components)
        = snd
        $ mapAccumL
            (\l item -> (l + either (const 1) textLength item, (l, item)))
            0
            components
