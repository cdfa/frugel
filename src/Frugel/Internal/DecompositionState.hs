{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.DecompositionState
    ( DecompositionState(DecompositionState)
    , initialDecompositionState
    ) where

import           Optics

-- A text offset of -1 is used for representing that we are done with parsing
-- because 0 could also mean we are at the start of a node that still needs to be decomposed
data DecompositionState
    = DecompositionState { cstrSiteOffset :: Int, textOffset :: Int }
    deriving ( Show, Eq )

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionState

initialDecompositionState :: Int -> DecompositionState

initialDecompositionState
    textOffset = DecompositionState { cstrSiteOffset = 0, .. }
