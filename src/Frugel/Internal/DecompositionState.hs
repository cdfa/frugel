{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Frugel.Internal.DecompositionState
    ( ModificationStatus(..)
    , DecompositionState(DecompositionState)
    , DecompositionMonad
    , initialDecompositionState
    , _Todo
    , _Success
    ) where

import           Optics

data ModificationStatus = Todo | Success
    deriving ( Show )

-- A text offset of -1 is used for representing that we are done with parsing
-- because 0 could also mean we are at the start of a node that still needs to be decomposed
data DecompositionState
    = DecompositionState { textOffset         :: Int
                         , cstrSiteOffset     :: Int
                         , modificationStatus :: ModificationStatus
                         }
    deriving ( Show )

type DecompositionMonad m = StateT DecompositionState m

makePrisms ''ModificationStatus

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionState

initialDecompositionState :: Int -> DecompositionState

initialDecompositionState textOffset
    = DecompositionState { cstrSiteOffset = 0, modificationStatus = Todo, .. }
