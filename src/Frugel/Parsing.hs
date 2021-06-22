{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Frugel.Parsing where

import qualified Data.Sequence as Seq

import Frugel.CstrSite
import Frugel.Decomposition

import Optics.Extra

class Parseable p where
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
