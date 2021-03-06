{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scout.Internal.EvaluationEnv where

import Control.Limited

import qualified Data.Map as Map
import qualified Data.Set as Set

import Optics

import Scout.Node

data EvaluationEnv
    = EvaluationEnv { valueEnv :: Map Identifier (EvaluationRef Expr)
                      -- used for tracking all bindings up to the first application (renameShadowedVariables takes over from there)
                    , shadowingEnv :: ShadowingEnv
                    , definitions :: Set Identifier
                    , initialFuel :: Limit
                    , skipNextOutOfFuel :: Bool
                    }

makeFieldLabelsNoPrefix ''EvaluationEnv

makePrisms ''EvaluationEnv

magnifyShadowingEnv :: Magnify m n EvaluationEnv ShadowingEnv
    => Identifier
    -> EvaluationRef Expr
    -> EvaluationEnv
    -> m c
    -> n c
magnifyShadowingEnv n arg EvaluationEnv{shadowingEnv = _, ..}
    = magnify . to $ \shadowingEnv ->
    EvaluationEnv { valueEnv = Map.insert n arg valueEnv
                  , shadowingEnv
                  , definitions = Set.delete n definitions
                  , initialFuel
                  , skipNextOutOfFuel
                  }
