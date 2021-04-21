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
    , DecompositionEnv(..)
    , DecompositionMonad
    , initialDecompositionState
    , _Todo
    , _Success
    , _Errors
    ) where

import           Frugel.Node
import           Frugel.PrettyPrinting

import           Optics

data ModificationStatus = Todo | Success | Errors [Doc Annotation]
    deriving ( Show )

-- A text offset of -1 is used for representing that we are done with parsing
-- because 0 could also mean we are at the start of a node that still needs to be decomposed
data DecompositionState
    = DecompositionState { textOffset         :: Int
                         , cstrSiteOffset     :: Int
                         , modificationStatus :: ModificationStatus
                         }
    deriving ( Show )

data DecompositionEnv
    = DecompositionEnv { mapChar        :: Char -> DecompositionMonad Char
                       , mapIdentifier
                             :: Identifier -> DecompositionMonad Identifier
                       , mapExpr        :: Expr -> DecompositionMonad Expr
                       , mapDecl        :: Decl -> DecompositionMonad Decl
                       , mapWhereClause
                             :: WhereClause -> DecompositionMonad WhereClause
                       }

type DecompositionMonad
    = ReaderT DecompositionEnv (StateT DecompositionState Identity)

makePrisms ''ModificationStatus

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionState

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionEnv

initialDecompositionState :: Int -> DecompositionState

initialDecompositionState textOffset
    = DecompositionState { cstrSiteOffset = 0, modificationStatus = Todo, .. }
