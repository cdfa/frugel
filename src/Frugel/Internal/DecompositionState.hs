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
    ) where

import           Frugel.Node

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

data DecompositionEnv m
    = DecompositionEnv { mapChar        :: Char -> (DecompositionMonad m) Char
                       , mapIdentifier  :: Identifier
                             -> (DecompositionMonad m) Identifier
                       , mapExpr        :: Expr -> (DecompositionMonad m) Expr
                       , mapDecl        :: Decl -> (DecompositionMonad m) Decl
                       , mapWhereClause :: WhereClause
                             -> (DecompositionMonad m) WhereClause
                       }

type DecompositionMonad m = ReaderT (DecompositionEnv m) m

makePrisms ''ModificationStatus

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionState

makeFieldLabelsWith noPrefixFieldLabels ''DecompositionEnv

initialDecompositionState :: Int -> DecompositionState

initialDecompositionState textOffset
    = DecompositionState { cstrSiteOffset = 0, modificationStatus = Todo, .. }
